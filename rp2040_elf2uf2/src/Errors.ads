--
--  Copyright 2022 (C) Nicolas Pinault (aka DrPi)
--
--  SPDX-License-Identifier: BSD-3-Clause
--

with Ada.Command_Line; use Ada.Command_Line;

package Errors is

   NO_ERROR     : constant Exit_Status :=  Success;
   ARGS         : constant Exit_Status := -1;
   FORMAT       : constant Exit_Status := -2;
   INCOMPATIBLE : constant Exit_Status := -3;
   READ_FAILED  : constant Exit_Status := -4;
   WRITE_FAILED : constant Exit_Status := -5;

   function Fail (Code : Exit_Status; Msg : String) return Exit_Status;
   function Fail_Read_Error return Exit_Status;
   function Fail_Write_Error return Exit_Status;

end Errors;