--
--  Copyright 2022 (C) Nicolas Pinault (aka DrPi)
--
--  SPDX-License-Identifier: BSD-3-Clause
--
--
--  Converts an ELF file to a UF2 formated file.
--
--  UF2 files are accepted by RP2040 micro_controllers 
--  in BOOTSEL mode for FLASH programming.
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Streams.Stream_IO;
with elf2uf2;
with Errors;

procedure Rp2040_Elf2uf2 is 

   package SIO renames Ada.Streams.Stream_IO;

   function usage return Exit_Status is
   begin
      Put_Line ("Usage: elf2uf2 (-v) <input ELF file> <output UF2 file>");
      return Errors.ARGS;
   end usage;

   Arg : Natural := 0;

begin
   if (Argument_Count < 2) or (Argument_Count > 3) then
      Set_Exit_Status (Usage);
      return;
   end if;

   if Argument_Count = 3 then
      Arg := 2;
      if (Argument(1) = "-v") then
         elf2uf2.Set_Verbosity(1);
      elsif (Argument(1) = "-vv") then
         elf2uf2.Set_Verbosity(2);
      else
         Set_Exit_Status (Usage);
         return;
      end if;
   else
      Arg := 1;
   end if;

   declare
      In_Filename  : constant String := Argument(Arg);
      Out_Filename : constant String := Argument(Arg+1);
      In_File  : SIO.File_Type;
      Out_File : SIO.File_Type;
   begin
      declare
      begin
         SIO.Open (File => In_File, Mode => SIO.In_File, Name => In_Filename);
      exception
         when Name_Error =>
            Put_Line ("Input File does not exist.");
            Set_Exit_Status (Errors.ARGS);
            return;
         when others =>
            Put_Line ("Error while opening input file.");
            Set_Exit_Status (Errors.ARGS);
            return;
      end;

      declare
      begin
         SIO.Create (File => Out_File, Mode => SIO.Out_File, Name => Out_Filename);
      exception
         when others =>
            Put_Line ("Error while creating Output file.");
            Set_Exit_Status (Errors.ARGS);
            return;
      end;

      declare
         Ret_Code : Exit_Status;
      begin
         Ret_Code := elf2uf2.Run(In_File  => In_File, 
                                 Out_File => Out_File, 
                                 Fill_Holes_With_Zeros => True);
         SIO.Close(In_File);
         SIO.Close(Out_File);
         if Ret_Code /= Errors.NO_ERROR then
            SIO.Delete (Out_File);
         end if;
         Set_Exit_Status (Ret_Code);
      end;
   end;
end Rp2040_Elf2uf2;