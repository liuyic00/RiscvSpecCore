package rvspeccore.core.tool

import chisel3._
import rvspeccore.core.BaseCore

trait CheckTool extends BaseCore{
  def updateNextWrite(rd_addr: UInt): Unit = {
    next.rd_en := true.B
    next.rd_addr := rd_addr
    next.rd_data := next.reg(rd_addr)
  }

  def checkSrcImm(rs1_addr: UInt): Unit = {
    now.reg(now.rs1_addr) := now.rs1_data
    next.rs1_addr := rs1_addr
    next.checkrs1 := true.B
  }

  def checkSrcReg(rs1_addr: UInt, rs2_addr: UInt): Unit = {
    now.reg(rs1_addr) := now.rs1_data
    now.reg(rs2_addr) := now.rs2_data
    next.rs1_addr := rs1_addr
    next.checkrs1 := true.B
    next.rs2_addr := rs2_addr
    next.checkrs2 := true.B
  }

  def updateNextCsrWrite(csr_addr: UInt): Unit = {
    next.csr_addr := csr_addr
    next.csr_wr := true.B
  }
}
