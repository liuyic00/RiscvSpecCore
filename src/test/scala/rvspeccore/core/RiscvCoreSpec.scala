package rvspeccore.core

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import chisel3.util.experimental.loadMemoryFromFile
import java.io.File

class CoreTester(memFile: String)(implicit config: RVConfig) extends Module {
  implicit val XLEN = config.XLEN

  val bytes      = XLEN / 8
  val bytesWidth = log2Ceil(bytes)

  val io = IO(new Bundle {
    val inst = Output(UInt(32.W))
    val now  = Output(State())
  })

  val mem  = Mem(3000, UInt(XLEN.W))
  val core = Module(new RiscvCore)

  loadMemoryFromFile(mem, memFile)

  // map pc "h8000_0000".U to "h0000_0000".U
  val pc   = core.io.now.pc - "h8000_0000".U
  val inst = Wire(UInt(32.W))

  // inst
  core.io.valid := !reset.asBool
  config match {
    case RV32Config() => {
      val instMem = mem.read(pc >> 2)
      inst := instMem
    }
    case RV64Config() => {
      val instMem = mem.read(pc >> 3)
      inst := Mux(pc(2), instMem(63, 32), instMem(31, 0))
    }
  }
  core.io.inst := inst

  def width2Mask(width: UInt): UInt = {
    MuxLookup(
      width,
      0.U(64.W),
      Array(
        8.U  -> "hff".U(64.W),
        16.U -> "hffff".U(64.W),
        32.U -> "hffff_ffff".U(64.W),
        64.U -> "hffff_ffff_ffff_ffff".U(64.W)
      )
    )
  }

  // read mem
  val rIdx  = core.io.mem.read.addr >> bytesWidth           // addr / (XLEN/8)
  val rOff  = core.io.mem.read.addr(bytesWidth - 1, 0) << 3 // addr(byteWidth-1,0) * 8
  val rMask = width2Mask(core.io.mem.read.memWidth)
  when(core.io.mem.read.valid) {
    core.io.mem.read.data := (mem.read(rIdx) >> rOff) & rMask
  } otherwise {
    core.io.mem.read.data := 0.U
  }

  // write mem
  val wIdx  = core.io.mem.write.addr >> bytesWidth           // addr / bytes
  val wOff  = core.io.mem.write.addr(bytesWidth - 1, 0) << 3 // addr(byteWidth-1,0) * 8
  val wMask = (width2Mask(core.io.mem.write.memWidth) << wOff)(XLEN - 1, 0)
  val mData = mem.read(wIdx)
  // simulate write mask
  val wData = ((core.io.mem.write.data << wOff)(XLEN - 1, 0) & wMask) | (mData & ~wMask)
  when(core.io.mem.write.valid) {
    mem.write(wIdx, wData)
  }

  io.inst := inst
  io.now  := core.io.now
}

object RiscvTests {
  val root = "testcase/riscv-tests-hex"
  def apply(instSet: String) = {
    val set = new File(root + "/" + instSet)
    set.listFiles().filter(_.getName().endsWith(".hex")).sorted
  }
}

class RiscvCoreSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "RiscvCore"
  it should "pass RV64Config firrtl emit" in {
    (new chisel3.stage.ChiselStage)
      .emitFirrtl(new RiscvCore()(RV64Config()), Array("--target-dir", "test_run_dir/" + getTestName))
  }
  it should "pass manual test" in {
    test(new RiscvCore()(RV64Config())).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.io.valid.poke(true.B)
      c.io.inst.poke("h0000006f".U)
      c.io.now.pc.expect("h8000_0000".U)
      c.io.next.pc.expect("h8000_0000".U)
    }
  }

  def stepTest(dut: CoreTester, restClock: Int): Int = {
    dut.clock.step(1)
    if (dut.io.inst.peek().litValue == "h0000006f".U.litValue) { // end
      restClock
    } else if (restClock <= 0) { // some thing wrong
      restClock
    } else { // next step
      stepTest(dut, restClock - 1)
    }
  }

  val tests = Seq(
    (RV32Config(), Seq("rv32ui")),
    (RV64Config(), Seq("rv64ui"))
  )
  // NOTE: funce.i shows passed test, but RiscvCore not support it.
  //       Because RiscvCore is too simple.
  tests.foreach { testInfo =>
    behavior of s"RiscvCore with ${testInfo._1.getClass().getSimpleName()}"
    testInfo._2.foreach { testCase =>
      RiscvTests(testCase).foreach(f =>
        it should s"pass ${f.getName}" in {
          test(
            new CoreTester(f.getCanonicalPath())(testInfo._1)
          ).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
            stepTest(c, 600)
            c.io.inst.expect("h0000006f".U(32.W)) // j halt
            c.io.now.reg(10).expect(0.U)          // li	a0,0
          }
        }
      )
    }
  }
}
