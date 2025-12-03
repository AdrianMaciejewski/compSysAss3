import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))

  })

  //Default definitions
  io.address := 0.U
  io.done := false.B
  io.writeEnable := false.B

  //States
  val idle :: init :: read :: write :: check :: find :: compare :: done :: edgeT :: edgeB :: edgeL :: edgeR :: Nil = Enum (12)
  val stateReg = RegInit(idle)

  //Register used to write data
  val outPxReg = RegInit(0.U(32.W))
  io.dataWrite := outPxReg

  //Load data into these registers
  val regs1 = RegInit(VecInit(Seq.fill(20)(0.U(32.W))))
  val regs2 = RegInit(VecInit(Seq.fill(20)(0.U(32.W))))
  val regs3 = RegInit(VecInit(Seq.fill(20)(0.U(32.W))))
  val rows = VecInit(Seq(regs1, regs2, regs3))

  //Used to compare neighbours
  val regU = RegInit(0.U(32.W))
  val regD = RegInit(0.U(32.W))
  val regL = RegInit(0.U(32.W))
  val regR = RegInit(0.U(32.W))

  val regX = RegInit(1.U(5.W)) // value from 1-20
  val coord = RegInit(21.U(16.W)) //the current pixel under investigation
  val rowSel = RegInit(0.U(2.W)) //wire to select regs from vector 'rows'

  val regLoadAddr = RegInit(0.U(16.W)) //similar to 'coords' but exclusively used for loading
  val regI = RegInit(0.U(5.W)) //similar to 'regX'

  switch(stateReg) {
    is(idle) {
      when(io.start) {
        stateReg := init
      }
    }

    is(init) {
      io.address := regLoadAddr
      switch(rowSel) {
        is(0.U) {regs1(regI) := io.dataRead}
        is(1.U) {regs2(regI) := io.dataRead}
        is(2.U) {regs3(regI) := io.dataRead}
      }
      regLoadAddr := regLoadAddr + 1.U(16.W) //This should be assigned after data memory read

      // for i in range(60): load(i). Switch selected row at 20 and 40. Continue to 'check' at 60
      when(regLoadAddr === 19.U) { //go to next row
        regI := 0.U
        rowSel := 1.U
        stateReg := init
      }.otherwise {
        when(regLoadAddr === 39.U) { //go to next row
          regI := 0.U
          rowSel := 2.U
          stateReg := init
        }.otherwise {
          when(regLoadAddr === 59.U) { //exit 'start'
            regI := 0.U
            rowSel := 1.U
            stateReg := check
          }.otherwise { //else keep loading
            regI := regI + 1.U(5.W)
            stateReg := init
          }
        }
      }
    }

    // For i in range(20): load(i). Afterwards go to check
    is(read) {
      io.address := regLoadAddr
      switch(rowSel) {
        is(0.U) { regs3(regI) := io.dataRead }
        is(1.U) { regs1(regI) := io.dataRead }
        is(2.U) { regs2(regI) := io.dataRead }
      }
      when(regI >= 20.U) { //if done loading
        when(coord >= 398.U(16.W)) { //if all rows are done, exit program
          coord := 400.U
          stateReg := edgeT
        } .otherwise{ //continue the program
          rowSel := Mux(rowSel === 2.U, 0.U, rowSel + 1.U) //cycle selection through 0, 1 and 2
          regI := 0.U
//          regLoadAddr := regLoadAddr + 1.U(5.W)
//          coord := coord + 1.U(16.W)
          stateReg := check
        }
      } .otherwise{ //keep loading
        regI := regI+1.U(5.W)
        regLoadAddr := regLoadAddr + 1.U(5.W)
        stateReg := read
      }
    }

    // for i in range(20): Write. Afterwards go to read
    is(write) {
      io.address := coord + 400.U(16.W)
      io.writeEnable := true.B

      //and do other stuff besides write
      regX := regX + 1.U(5.W)
      when(regX === 19.U(5.W)) {
        regX := 1.U(5.W)
        coord := coord + 2.U(16.W)
        stateReg := read
      } .otherwise{
        coord := coord + 1.U(16.W)
        stateReg := check
      }
    }

    // Check whether current pixel is black. If so return with output 0
    is(check) {
      val px = MuxLookup(rowSel, 0.U)(Seq(
        0.U -> regs1(regX),
        1.U -> regs2(regX),
        2.U -> regs3(regX),
      ))

      when(px===0.U) {
        outPxReg := 0.U
        stateReg := write
      } .otherwise{
        stateReg := find
      }
    }

    // Find neighbour pixels
    is(find) {
      regL := MuxLookup(rowSel, 0.U)(Seq(
        0.U -> regs1(regX-1.U),
        1.U -> regs2(regX-1.U),
        2.U -> regs3(regX-1.U),
      ))
      regR := MuxLookup(rowSel, 0.U)(Seq(
        0.U -> regs1(regX+1.U),
        1.U -> regs2(regX+1.U),
        2.U -> regs3(regX+1.U),
      ))
      regU := MuxLookup(rowSel, 0.U)(Seq(
        0.U -> regs3(regX),
        1.U -> regs1(regX),
        2.U -> regs2(regX),
      ))
      regD := MuxLookup(rowSel, 0.U)(Seq(
        0.U -> regs2(regX),
        1.U -> regs3(regX),
        2.U -> regs1(regX),
      ))

      stateReg := compare
    }

    // Erode if one neighbour is black
    is(compare) {
      outPxReg := Mux(regU===0.U(32.W) || regD===0.U(32.W) || regL===0.U(32.W) || regR===0.U(32.W), 0.U(32.W), 255.U(32.W))
      stateReg := write
    }

    is(edgeT){
      outPxReg := 0.U
      io.writeEnable := true.B
      coord := coord + 1.U
      io.address := coord
      when(coord === 419.U) {
        stateReg := edgeB
        coord := 780.U
      } // otherwise( stateReg := edgeT) maybe?
    }

    is(edgeB){
      outPxReg := 0.U
      io.writeEnable := true.B
      coord := coord + 1.U
      io.address := coord
      when(coord === 799.U) {
        stateReg := edgeL
        coord := 420.U
      }
    }

    is(edgeL){
      outPxReg := 0.U
      io.writeEnable := true.B
      coord := coord + 20.U
      io.address := coord
      when(coord === 760.U) {
        stateReg := edgeR
        coord := 439.U
      }
    }

    is(edgeR) {
      outPxReg := 0.U
      io.writeEnable := true.B
      coord := coord + 20.U
      io.address := coord
      when(coord === 779.U) {
        stateReg := done
      }
    }

    is(done) {
      io.done := true.B
      // remain in done state
    }
  }

}
