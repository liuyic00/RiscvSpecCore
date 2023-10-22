/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
* 
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2. 
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2 
* 
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER 
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR 
* FIT FOR A PARTICULAR PURPOSE.  
*
* See the Mulan PSL v2 for more details.  
***************************************************************************************/

package rvspeccore.device

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

import rvspeccore.bus.axi4._
import rvspeccore.utils._

class RAMHelper(memByte: Int) extends BlackBox {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val rIdx = Input(UInt(64.W))
    val rdata = Output(UInt(64.W))
    val wIdx = Input(UInt(64.W))
    val wdata = Input(UInt(64.W))
    val wmask = Input(UInt(64.W))
    val wen = Input(Bool())
    val en = Input(Bool())
  }).suggestName("io")
}

class AXI4RAM[T <: AXI4Lite](_type: T = new AXI4, memByte: Int,
  useBlackBox: Boolean = false, memFile: String) extends AXI4SlaveModule(_type) {

  val offsetBits = log2Up(memByte)
  val offsetMask = (1 << offsetBits) - 1
  def index(addr: UInt) = (addr & offsetMask.U) >> log2Ceil(4)
  def inRange(idx: UInt) = idx < (memByte / 8).U

  val wIdx = index(waddr) + writeBeatCnt
  val rIdx = index(raddr) + readBeatCnt
  val wen = in.w.fire && inRange(wIdx)

  val rdata = if (useBlackBox) {
    val mem = Module(new RAMHelper(memByte))
    mem.io.clk := clock
    mem.io.rIdx := rIdx
    mem.io.wIdx := wIdx
    mem.io.wdata := in.w.bits.data
    mem.io.wmask := fullMask
    mem.io.wen := wen
    mem.io.en := true.B
    mem.io.rdata
  } else {
    val mem = Mem(memByte, UInt(32.W))
    
    if (memFile != ""){
      loadMemoryFromFile(mem, memFile)
    }

    // Firstly enable to read
    // FIXME: Can not write anymore
    // val wdata = VecInit.tabulate(8) { i => in.w.bits.data(8*(i+1)-1, 8*i) }
    // // print in.w.fire and inRange(wIdx)
    // printf("[AXI4RAM] in.w.ready = %d, in.w.valid = %d, inRange(wIdx) = %d\n", in.w.ready, in.w.valid, inRange(wIdx))
    // when (wen) { 
    //   // print widx and wdata
    //   printf("[AXI4RAM------] wIdx = %d, wdata = %d\n", wIdx, in.w.bits.data)
    //   // mem.write(wIdx, wdata, in.w.bits.strb.asBools) 
    // }
    Cat(mem.read(rIdx))
  }
  when(ren) {
    // printf("[AXI4RAM] raddr = %d rIdx = %d, rdata = %d\n", raddr, rIdx, rdata)
    // print in.r valid and ready
  }
  in.r.bits.data := RegEnable(rdata, ren)
}