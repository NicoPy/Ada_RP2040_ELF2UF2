--
--  Copyright 2022 (C) Nicolas Pinault (aka DrPi)
--
--  SPDX-License-Identifier: BSD-3-Clause
--

with Interfaces; use Interfaces;

package Elf is

   ELF_MAGIC : constant := 16#464c457f#;

   EM_ARM : constant := 16#28#;

   PT_LOAD : constant := 16#00000001#;

   type Elf_Header_Pad_Array is array (1 .. 7) of Unsigned_8
      with Size => 7*8;

   type Elf_Header is record
      Magic       : Unsigned_32;
      Arch_Class  : Unsigned_8;
      Endianness  : Unsigned_8;
      Version     : Unsigned_8;
      Abi         : Unsigned_8;
      Abi_Version : Unsigned_8;
      Pad         : Elf_Header_Pad_Array;
      Header_Type : Unsigned_16;
      Machine     : Unsigned_16;
      Version2    : Unsigned_32;
   end record
      with Size => (1*4 + 5 + 7 + 2*2 + 1*4)*8;

   for Elf_Header use record
      Magic       at  0 range 0 .. 31;
      Arch_Class  at  4 range 0 .. 7;
      Endianness  at  5 range 0 .. 7;
      Version     at  6 range 0 .. 7;
      Abi         at  7 range 0 .. 7;
      Abi_Version at  8 range 0 .. 7;
      Pad         at  9 range 0 .. 55;
      Header_Type at 16 range 0 .. 15;
      Machine     at 18 range 0 .. 15;
      Version2    at 20 range 0 .. 31;
   end record;


   EF_ARM_ABI_FLOAT_HARD : constant := 16#00000400#;

   type Elf32_Header is record
      Common        : Elf_Header;
      Prog_Entry    : Unsigned_32;
      Ph_Offset     : Unsigned_32;
      Sh_Offset     : Unsigned_32;
      Flags         : Unsigned_32;
      Eh_Size       : Unsigned_16;
      Ph_Entry_Size : Unsigned_16;
      Ph_Num        : Unsigned_16;
      Sh_Entry_Size : Unsigned_16;
      Sh_Num        : Unsigned_16;
      Sh_Str_Index  : Unsigned_16;
   end record
      with Size => (24 + 4*4 + 6*2)*8;

   for Elf32_Header use record
      Common        at  0 range 0 .. 24*8-1;
      Prog_Entry    at 24 range 0 .. 31;
      Ph_Offset     at 28 range 0 .. 31;
      Sh_Offset     at 32 range 0 .. 31;
      Flags         at 36 range 0 .. 31;
      Eh_Size       at 40 range 0 .. 15;
      Ph_Entry_Size at 42 range 0 .. 15;
      Ph_Num        at 44 range 0 .. 15;
      Sh_Entry_Size at 46 range 0 .. 15;
      Sh_Num        at 48 range 0 .. 15;
      Sh_Str_Index  at 50 range 0 .. 15;
   end record;

   type Elf32_Header_Access is access Elf32_Header;

   type Elf32_Ph_Entry is record
      Entry_Type : Unsigned_32;
      Offset     : Unsigned_32;
      Vaddr      : Unsigned_32;
      Paddr      : Unsigned_32;
      Filez      : Unsigned_32;
      Memsz      : Unsigned_32;
      Flags      : Unsigned_32;
      Align      : Unsigned_32;
   end record
      with Size => 8*4*8;

   for Elf32_Ph_Entry use record
      Entry_Type at  0 range 0 .. 31;
      Offset     at  4 range 0 .. 31;
      Vaddr      at  8 range 0 .. 31;
      Paddr      at 12 range 0 .. 31;
      Filez      at 16 range 0 .. 31;
      Memsz      at 20 range 0 .. 31;
      Flags      at 24 range 0 .. 31;
      Align      at 28 range 0 .. 31;
   end record;
   
   type Elf32_Ph_Entry_Array is array (Natural range <>) of Elf32_Ph_Entry;

end Elf;