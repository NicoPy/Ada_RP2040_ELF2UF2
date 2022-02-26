--
--  Copyright 2022 (C) Nicolas Pinault (aka DrPi)
--
--  SPDX-License-Identifier: BSD-3-Clause
--

with Ada.Text_IO; use Ada.Text_IO;

package body Errors is

   function Fail (Code : Exit_Status; Msg : String) return Exit_Status is
   begin
      Put_Line (Msg);
      return Code;
   end Fail;

   function Fail_Read_Error return Exit_Status is
   begin
      return Fail (READ_FAILED, "Failed to read input file.");
   end Fail_Read_Error;

   function Fail_Write_Error return Exit_Status is
   begin
      return Fail (WRITE_FAILED, "Failed to write output file.");
   end Fail_Write_Error;

end Errors;