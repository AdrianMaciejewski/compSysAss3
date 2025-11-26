FIRRTL version 1.2.0
circuit Hello :
  module DataMemory :
    input clock : Clock
    input reset : UInt<1>
    input io_address : UInt<16> @[\\src\\main\\scala\\DataMemory.scala 5:14]
    output io_dataRead : UInt<32> @[\\src\\main\\scala\\DataMemory.scala 5:14]
    input io_writeEnable : UInt<1> @[\\src\\main\\scala\\DataMemory.scala 5:14]
    input io_dataWrite : UInt<32> @[\\src\\main\\scala\\DataMemory.scala 5:14]
    input io_testerEnable : UInt<1> @[\\src\\main\\scala\\DataMemory.scala 5:14]
    input io_testerAddress : UInt<16> @[\\src\\main\\scala\\DataMemory.scala 5:14]
    output io_testerDataRead : UInt<32> @[\\src\\main\\scala\\DataMemory.scala 5:14]
    input io_testerWriteEnable : UInt<1> @[\\src\\main\\scala\\DataMemory.scala 5:14]
    input io_testerDataWrite : UInt<32> @[\\src\\main\\scala\\DataMemory.scala 5:14]

    mem memory : @[\\src\\main\\scala\\DataMemory.scala 19:20]
      data-type => UInt<32>
      depth => 65536
      read-latency => 0
      write-latency => 1
      reader => io_testerDataRead_MPORT
      reader => io_dataRead_MPORT
      writer => MPORT
      writer => MPORT_1
      read-under-write => undefined
    node _GEN_0 = validif(io_testerWriteEnable, io_testerAddress) @[\\src\\main\\scala\\DataMemory.scala 28:32]
    node _GEN_1 = validif(io_testerWriteEnable, clock) @[\\src\\main\\scala\\DataMemory.scala 28:32]
    node _GEN_2 = mux(io_testerWriteEnable, UInt<1>("h1"), UInt<1>("h0")) @[\\src\\main\\scala\\DataMemory.scala 19:20 28:32]
    node _GEN_3 = validif(io_testerWriteEnable, UInt<1>("h1")) @[\\src\\main\\scala\\DataMemory.scala 28:32]
    node _GEN_4 = validif(io_testerWriteEnable, io_testerDataWrite) @[\\src\\main\\scala\\DataMemory.scala 28:32]
    node _GEN_5 = mux(io_testerWriteEnable, io_testerDataWrite, memory.io_testerDataRead_MPORT.data) @[\\src\\main\\scala\\DataMemory.scala 25:23 28:32 31:25]
    node _GEN_6 = validif(io_writeEnable, io_address) @[\\src\\main\\scala\\DataMemory.scala 38:26]
    node _GEN_7 = validif(io_writeEnable, clock) @[\\src\\main\\scala\\DataMemory.scala 38:26]
    node _GEN_8 = mux(io_writeEnable, UInt<1>("h1"), UInt<1>("h0")) @[\\src\\main\\scala\\DataMemory.scala 19:20 38:26]
    node _GEN_9 = validif(io_writeEnable, UInt<1>("h1")) @[\\src\\main\\scala\\DataMemory.scala 38:26]
    node _GEN_10 = validif(io_writeEnable, io_dataWrite) @[\\src\\main\\scala\\DataMemory.scala 38:26]
    node _GEN_11 = mux(io_writeEnable, io_dataWrite, memory.io_dataRead_MPORT.data) @[\\src\\main\\scala\\DataMemory.scala 35:17 38:26 41:19]
    node _GEN_12 = validif(io_testerEnable, io_testerAddress) @[\\src\\main\\scala\\DataMemory.scala 23:24 25:37]
    node _GEN_13 = validif(io_testerEnable, clock) @[\\src\\main\\scala\\DataMemory.scala 23:24 25:37]
    node _GEN_14 = mux(io_testerEnable, UInt<1>("h1"), UInt<1>("h0")) @[\\src\\main\\scala\\DataMemory.scala 19:20 23:24 25:37]
    node _GEN_15 = mux(io_testerEnable, _GEN_5, UInt<32>("h0")) @[\\src\\main\\scala\\DataMemory.scala 23:24 37:23]
    node _GEN_16 = mux(io_testerEnable, UInt<32>("h0"), _GEN_11) @[\\src\\main\\scala\\DataMemory.scala 23:24 27:17]
    node _GEN_17 = validif(io_testerEnable, _GEN_0) @[\\src\\main\\scala\\DataMemory.scala 23:24]
    node _GEN_18 = validif(io_testerEnable, _GEN_1) @[\\src\\main\\scala\\DataMemory.scala 23:24]
    node _GEN_19 = mux(io_testerEnable, _GEN_2, UInt<1>("h0")) @[\\src\\main\\scala\\DataMemory.scala 19:20 23:24]
    node _GEN_20 = validif(io_testerEnable, _GEN_3) @[\\src\\main\\scala\\DataMemory.scala 23:24]
    node _GEN_21 = validif(io_testerEnable, _GEN_4) @[\\src\\main\\scala\\DataMemory.scala 23:24]
    node _GEN_22 = validif(eq(io_testerEnable, UInt<1>("h0")), io_address) @[\\src\\main\\scala\\DataMemory.scala 23:24 35:31]
    node _GEN_23 = validif(eq(io_testerEnable, UInt<1>("h0")), clock) @[\\src\\main\\scala\\DataMemory.scala 23:24 35:31]
    node _GEN_24 = mux(io_testerEnable, UInt<1>("h0"), UInt<1>("h1")) @[\\src\\main\\scala\\DataMemory.scala 19:20 23:24 35:31]
    node _GEN_25 = validif(eq(io_testerEnable, UInt<1>("h0")), _GEN_6) @[\\src\\main\\scala\\DataMemory.scala 23:24]
    node _GEN_26 = validif(eq(io_testerEnable, UInt<1>("h0")), _GEN_7) @[\\src\\main\\scala\\DataMemory.scala 23:24]
    node _GEN_27 = mux(io_testerEnable, UInt<1>("h0"), _GEN_8) @[\\src\\main\\scala\\DataMemory.scala 19:20 23:24]
    node _GEN_28 = validif(eq(io_testerEnable, UInt<1>("h0")), _GEN_9) @[\\src\\main\\scala\\DataMemory.scala 23:24]
    node _GEN_29 = validif(eq(io_testerEnable, UInt<1>("h0")), _GEN_10) @[\\src\\main\\scala\\DataMemory.scala 23:24]
    io_dataRead <= _GEN_16
    io_testerDataRead <= _GEN_15
    memory.io_testerDataRead_MPORT.addr <= _GEN_12
    memory.io_testerDataRead_MPORT.en <= _GEN_14
    memory.io_testerDataRead_MPORT.clk <= _GEN_13
    memory.io_dataRead_MPORT.addr <= _GEN_22
    memory.io_dataRead_MPORT.en <= _GEN_24
    memory.io_dataRead_MPORT.clk <= _GEN_23
    memory.MPORT.addr <= _GEN_17
    memory.MPORT.en <= _GEN_19
    memory.MPORT.clk <= _GEN_18
    memory.MPORT.data <= _GEN_21
    memory.MPORT.mask <= _GEN_20
    memory.MPORT_1.addr <= _GEN_25
    memory.MPORT_1.en <= _GEN_27
    memory.MPORT_1.clk <= _GEN_26
    memory.MPORT_1.data <= _GEN_29
    memory.MPORT_1.mask <= _GEN_28

  module Hello :
    input clock : Clock
    input reset : UInt<1>
    output io_done : UInt<1> @[\\src\\main\\scala\\Hello.scala 21:14]
    input io_start : UInt<1> @[\\src\\main\\scala\\Hello.scala 21:14]
    input io_testerDataMemEnable : UInt<1> @[\\src\\main\\scala\\Hello.scala 21:14]
    input io_testerDataMemAddress : UInt<16> @[\\src\\main\\scala\\Hello.scala 21:14]
    output io_testerDataMemDataRead : UInt<32> @[\\src\\main\\scala\\Hello.scala 21:14]
    input io_testerDataMemWriteEnable : UInt<1> @[\\src\\main\\scala\\Hello.scala 21:14]
    input io_testerDataMemDataWrite : UInt<32> @[\\src\\main\\scala\\Hello.scala 21:14]

    inst dataMemory of DataMemory @[\\src\\main\\scala\\Hello.scala 33:26]
    reg stateReg : UInt<2>, clock with :
      reset => (UInt<1>("h0"), stateReg) @[\\src\\main\\scala\\Hello.scala 37:25]
    reg addressReg : UInt<16>, clock with :
      reset => (UInt<1>("h0"), addressReg) @[\\src\\main\\scala\\Hello.scala 40:27]
    reg dataReg : UInt<32>, clock with :
      reset => (UInt<1>("h0"), dataReg) @[\\src\\main\\scala\\Hello.scala 41:24]
    node _T = eq(UInt<2>("h0"), stateReg) @[\\src\\main\\scala\\Hello.scala 50:20]
    node _GEN_0 = mux(io_start, UInt<2>("h1"), stateReg) @[\\src\\main\\scala\\Hello.scala 52:22 53:18 37:25]
    node _GEN_1 = mux(io_start, UInt<16>("h0"), addressReg) @[\\src\\main\\scala\\Hello.scala 52:22 54:20 40:27]
    node _T_1 = eq(UInt<2>("h1"), stateReg) @[\\src\\main\\scala\\Hello.scala 50:20]
    node _dataReg_T = bits(dataMemory.io_dataRead, 7, 0) @[\\src\\main\\scala\\Hello.scala 60:56]
    node _dataReg_T_1 = not(_dataReg_T) @[\\src\\main\\scala\\Hello.scala 60:33]
    node _dataReg_T_2 = cat(UInt<24>("h0"), _dataReg_T_1) @[\\src\\main\\scala\\Hello.scala 60:21]
    node _T_2 = eq(UInt<2>("h2"), stateReg) @[\\src\\main\\scala\\Hello.scala 50:20]
    node _dataMemory_io_address_T = add(addressReg, UInt<16>("h190")) @[\\src\\main\\scala\\Hello.scala 65:43]
    node _dataMemory_io_address_T_1 = tail(_dataMemory_io_address_T, 1) @[\\src\\main\\scala\\Hello.scala 65:43]
    node _addressReg_T = add(addressReg, UInt<16>("h1")) @[\\src\\main\\scala\\Hello.scala 67:32]
    node _addressReg_T_1 = tail(_addressReg_T, 1) @[\\src\\main\\scala\\Hello.scala 67:32]
    node _T_3 = eq(addressReg, UInt<16>("h18f")) @[\\src\\main\\scala\\Hello.scala 68:23]
    node _GEN_2 = mux(_T_3, UInt<2>("h3"), UInt<2>("h1")) @[\\src\\main\\scala\\Hello.scala 68:40 69:18 71:18]
    node _T_4 = eq(UInt<2>("h3"), stateReg) @[\\src\\main\\scala\\Hello.scala 50:20]
    node _GEN_3 = mux(_T_4, UInt<1>("h1"), UInt<1>("h0")) @[\\src\\main\\scala\\Hello.scala 47:11 50:20 76:15]
    node _GEN_4 = mux(_T_4, UInt<2>("h3"), stateReg) @[\\src\\main\\scala\\Hello.scala 50:20 77:16 37:25]
    node _GEN_5 = mux(_T_2, _dataMemory_io_address_T_1, UInt<16>("h0")) @[\\src\\main\\scala\\Hello.scala 50:20 45:25 65:29]
    node _GEN_6 = mux(_T_2, UInt<1>("h1"), UInt<1>("h0")) @[\\src\\main\\scala\\Hello.scala 50:20 44:29 66:33]
    node _GEN_7 = mux(_T_2, _addressReg_T_1, addressReg) @[\\src\\main\\scala\\Hello.scala 50:20 67:18 40:27]
    node _GEN_8 = mux(_T_2, _GEN_2, _GEN_4) @[\\src\\main\\scala\\Hello.scala 50:20]
    node _GEN_9 = mux(_T_2, UInt<1>("h0"), _GEN_3) @[\\src\\main\\scala\\Hello.scala 47:11 50:20]
    node _GEN_10 = mux(_T_1, addressReg, _GEN_5) @[\\src\\main\\scala\\Hello.scala 50:20 59:29]
    node _GEN_11 = mux(_T_1, _dataReg_T_2, dataReg) @[\\src\\main\\scala\\Hello.scala 50:20 60:15 41:24]
    node _GEN_12 = mux(_T_1, UInt<2>("h2"), _GEN_8) @[\\src\\main\\scala\\Hello.scala 50:20 61:16]
    node _GEN_13 = mux(_T_1, UInt<1>("h0"), _GEN_6) @[\\src\\main\\scala\\Hello.scala 50:20 44:29]
    node _GEN_14 = mux(_T_1, addressReg, _GEN_7) @[\\src\\main\\scala\\Hello.scala 50:20 40:27]
    node _GEN_15 = mux(_T_1, UInt<1>("h0"), _GEN_9) @[\\src\\main\\scala\\Hello.scala 47:11 50:20]
    node _GEN_16 = mux(_T, _GEN_0, _GEN_12) @[\\src\\main\\scala\\Hello.scala 50:20]
    node _GEN_17 = mux(_T, _GEN_1, _GEN_14) @[\\src\\main\\scala\\Hello.scala 50:20]
    node _GEN_18 = mux(_T, UInt<16>("h0"), _GEN_10) @[\\src\\main\\scala\\Hello.scala 50:20 45:25]
    node _GEN_19 = mux(_T, dataReg, _GEN_11) @[\\src\\main\\scala\\Hello.scala 50:20 41:24]
    node _GEN_20 = mux(_T, UInt<1>("h0"), _GEN_13) @[\\src\\main\\scala\\Hello.scala 50:20 44:29]
    node _GEN_21 = mux(_T, UInt<1>("h0"), _GEN_15) @[\\src\\main\\scala\\Hello.scala 47:11 50:20]
    io_done <= _GEN_21
    io_testerDataMemDataRead <= dataMemory.io_testerDataRead @[\\src\\main\\scala\\Hello.scala 83:28]
    dataMemory.clock <= clock
    dataMemory.reset <= reset
    dataMemory.io_address <= _GEN_18
    dataMemory.io_writeEnable <= _GEN_20
    dataMemory.io_dataWrite <= dataReg @[\\src\\main\\scala\\Hello.scala 46:27]
    dataMemory.io_testerEnable <= io_testerDataMemEnable @[\\src\\main\\scala\\Hello.scala 85:30]
    dataMemory.io_testerAddress <= io_testerDataMemAddress @[\\src\\main\\scala\\Hello.scala 82:31]
    dataMemory.io_testerWriteEnable <= io_testerDataMemWriteEnable @[\\src\\main\\scala\\Hello.scala 86:35]
    dataMemory.io_testerDataWrite <= io_testerDataMemDataWrite @[\\src\\main\\scala\\Hello.scala 84:33]
    stateReg <= mux(reset, UInt<2>("h0"), _GEN_16) @[\\src\\main\\scala\\Hello.scala 37:{25,25}]
    addressReg <= mux(reset, UInt<16>("h0"), _GEN_17) @[\\src\\main\\scala\\Hello.scala 40:{27,27}]
    dataReg <= mux(reset, UInt<32>("h0"), _GEN_19) @[\\src\\main\\scala\\Hello.scala 41:{24,24}]
