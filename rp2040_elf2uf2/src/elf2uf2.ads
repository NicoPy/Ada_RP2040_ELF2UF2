--
--  Copyright 2022 (C) Nicolas Pinault (aka DrPi)
--
--  SPDX-License-Identifier: BSD-3-Clause
--

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Streams.Stream_IO;

package elf2uf2 is

   package SIO renames Ada.Streams.Stream_IO;
   use SIO;

   type Verbosity_Level is range 0 .. 2;

   procedure Set_Verbosity (Level : Verbosity_Level);

   function Run (In_File  : SIO.File_Type;
                 Out_File : in out SIO.File_Type) return Exit_Status;

private

   Verbosity : Verbosity_Level := 0;

end elf2uf2;