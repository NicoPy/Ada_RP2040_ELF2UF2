--
--  Copyright 2022 (C) Nicolas Pinault (aka DrPi)
--
--  SPDX-License-Identifier: BSD-3-Clause
--

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Elf;
with Uf2;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with Errors;


package body elf2uf2 is

   --**********************************
   type Address_Range_Type is (CONTENTS, NO_CONTENTS, IGNORE);

   type Address_Range is record
      From : Unsigned_32 := 0;
      To   : Unsigned_32 := 0;
      AR_Type : Address_Range_Type := IGNORE;
   end record;

   type Address_Range_Array is array (Natural range <>) of Address_Range;
   type Address_Range_Array_Access is access constant Address_Range_Array;

   MAIN_RAM_START        : constant := 16#20000000#;
   MAIN_RAM_END          : constant := 16#20042000#;
   FLASH_START           : constant := 16#10000000#;
   FLASH_END             : constant := 16#15000000#;
   XIP_SRAM_START        : constant := 16#15000000#;
   XIP_SRAM_END          : constant := 16#15004000#;
   MAIN_RAM_BANKED_START : constant := 16#21000000#;
   MAIN_RAM_BANKED_END   : constant := 16#21040000#;

   RP2040_Address_Ranges_Flash : aliased constant Address_Range_Array := 
      (
         (From => FLASH_START,           To => FLASH_END,           AR_Type => CONTENTS),
         (From => MAIN_RAM_START,        To => MAIN_RAM_END,        AR_Type => NO_CONTENTS),
         (From => MAIN_RAM_BANKED_START, To => MAIN_RAM_BANKED_END, AR_Type => NO_CONTENTS)
      );

   RP2040_Address_Ranges_Ram : aliased constant Address_Range_Array := 
      (
         (From => MAIN_RAM_START, To => MAIN_RAM_END, AR_Type => CONTENTS),
         (From => XIP_SRAM_START, To => XIP_SRAM_END, AR_Type => CONTENTS),
         (From => 16#00000000#,   To => 16#00004000#, AR_Type => IGNORE)       -- for now we ignore the BootRom if present
      );

   --**********************************
   -- We require 256 (as this is the page size supported by the device)
   LOG2_PAGE_SIZE : constant := 8;
   PAGE_SIZE      : constant := 2**LOG2_PAGE_SIZE;

   type Page_Fragment is record
      File_Offset : Unsigned_32;
      Page_Offset : Unsigned_32;
      NbBytes     : Unsigned_32;
   end record;

   package Fragment_Vector is new Ada.Containers.Vectors (Index_Type   => Natural,
                                                          Element_Type => Page_Fragment);

   -- function "=" (F1 : Fragment_Vector.Vector; F2 : Fragment_Vector.Vector) return Boolean is (True)
   --    with Unreferenced => F1, F2;
   function "=" (F1 : Fragment_Vector.Vector; F2 : Fragment_Vector.Vector) return Boolean;
   package Page_Map is new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => Unsigned_32,
                                                                   Element_Type => Fragment_Vector.Vector);


   --**********************************
   function Read_And_Check_Elf32_Header (In_File   : SIO.File_Type; 
                                         ElfHeader : in out Elf.Elf32_Header) return Exit_Status;

   function Read_And_Check_Elf32_Ph_Entries (In_File   : SIO.File_Type; 
                                             ElfHeader : in Elf.Elf32_Header;
                                             Valid_Ranges : Address_Range_Array;
                                             Pages : in out Page_Map.Map) return Exit_Status;

   function Is_Address_Valid (Addr : Unsigned_32;
                              Valid_Ranges : Address_Range_Array) return Boolean;

   function Is_Address_Initialized (Addr : Unsigned_32;
                                    Valid_Ranges : Address_Range_Array) return Boolean;
   
   function Is_Address_Mapped (Addr  : Unsigned_32;
                               Pages : Page_Map.Map) return Boolean;

   function Check_Address_Range (Valid_Ranges  : Address_Range_Array;
                                 Addr          : Unsigned_32;
                                 VAddr         : Unsigned_32;
                                 Size          : Unsigned_32;
                                 Uninitialized : Boolean;
                                 Addr_Range    : out Address_Range) return Exit_Status;

   function Realize_Page (In_File   : SIO.File_Type;
                          Fragments : Fragment_Vector.Vector;
                          Block     : in out Uf2.uf2_block) return Exit_Status
      with Pre => Block.Data'Length >= PAGE_SIZE;

   function Hex (Data : Unsigned_32) return String;

   --**********************************
   procedure Set_Verbosity (Level : Verbosity_Level) is
   begin
      Verbosity := Level;
   end Set_Verbosity;


   --**********************************
   function Run (In_File  : SIO.File_Type; 
                 Out_File : in out SIO.File_Type) return Exit_Status is
      ElfHeader    : Elf.Elf32_Header;
      Ram_Style    : Boolean := False;
      Valid_Ranges : Address_Range_Array_Access;
      Pages        : Page_Map.Map;
      Ret_Code     : Exit_Status;
   begin
      Ret_Code := Read_And_Check_Elf32_Header (In_File, ElfHeader);
      if Ret_Code = Errors.NO_ERROR then
         Ram_Style := Is_Address_Initialized (Addr => ElfHeader.Prog_Entry, Valid_Ranges => RP2040_Address_Ranges_Ram);
         if Ram_Style then
            Valid_Ranges := RP2040_Address_Ranges_Ram'Access;
            if Verbosity >= 1 then
               Put_Line ("Detected RAM binary");
            end if;
         else
            Valid_Ranges := RP2040_Address_Ranges_Flash'Access;
            if Verbosity >= 1 then
               Put_Line ("Detected FLASH binary");
            end if;
         end if;

         Ret_Code := Read_And_Check_Elf32_Ph_Entries(In_File, ElfHeader, Valid_Ranges.all, Pages);
      end if;

      if Ret_Code /= Errors.NO_ERROR then
         return Ret_Code;
      end if;

      if Pages.Is_Empty then
         return Errors.Fail(Errors.INCOMPATIBLE, "The input file has no memory pages");
      end if;

      if verbosity >= 1 then 
         Put_Line ("Pages Count " & Integer'Image(Integer(Pages.Length)));
      end if;

      if Ram_Style then
         declare
            expected_ep_main_ram : Unsigned_32 := Unsigned_32'Last;
            expected_ep_xip_sram : Unsigned_32 := Unsigned_32'Last;
            expected_ep : Unsigned_32;
         begin
            for Cursor in Pages.Iterate loop
               if ((Unsigned_32(Page_Map.Key(Cursor)) >= MAIN_RAM_START) and 
                   (Unsigned_32(Page_Map.Key(Cursor)) <  MAIN_RAM_END)) 
                   and (Unsigned_32(Page_Map.Key(Cursor)) < expected_ep_main_ram) then
                  expected_ep_main_ram := Unsigned_32(Page_Map.Key(Cursor)) or 1;
                  
               elsif ((Unsigned_32(Page_Map.Key(Cursor)) >= XIP_SRAM_START) and 
                      (Unsigned_32(Page_Map.Key(Cursor)) <  XIP_SRAM_END)) 
                      and (Unsigned_32(Page_Map.Key(Cursor)) < expected_ep_xip_sram) then
                  expected_ep_xip_sram := Unsigned_32(Page_Map.Key(Cursor)) or 1;
               end if;
            end loop;

            if expected_ep_main_ram /= Unsigned_32'Last then
               expected_ep := expected_ep_main_ram;
            else
               expected_ep := expected_ep_xip_sram;
            end if;

            if ElfHeader.Prog_Entry = expected_ep_xip_sram then
               return Errors.Fail(Errors.INCOMPATIBLE, "B0/B1 Boot ROM does not support direct entry into XIP_SRAM");
            elsif ElfHeader.Prog_Entry /= expected_ep then
               return Errors.Fail(Errors.INCOMPATIBLE, "A RAM binary should have an entry point at the beginning: " &
                                                       Hex (expected_ep) & "(not " & Hex(ElfHeader.Prog_Entry) & ")");
            end if;

            pragma Assert((Unsigned_32(MAIN_RAM_START) and (PAGE_SIZE - 1)) = 0);   -- Check MAIN_RAM_START is aligned on a page

            -- Currently don't require this as entry point is now at the start, we don't know where reset vector is
            if False then
               declare 
                  Block : Uf2.Uf2_Block;
               begin
                  Ret_Code := Realize_Page (In_File, Pages(MAIN_RAM_START), Block);
                  if Ret_Code /= Errors.NO_ERROR then
                     return Ret_Code;
                  end if;

                  declare
                     SP : constant Unsigned_32 := Shift_Left(Unsigned_32(Block.Data(0)),  0) +
                                                  Shift_Left(Unsigned_32(Block.Data(1)),  8) +
                                                  Shift_Left(Unsigned_32(Block.Data(2)), 16) +
                                                  Shift_Left(Unsigned_32(Block.Data(3)), 24) ;
                     IP : constant Unsigned_32 := Shift_Left(Unsigned_32(Block.Data(4)),  0) +
                                                  Shift_Left(Unsigned_32(Block.Data(5)),  8) +
                                                  Shift_Left(Unsigned_32(Block.Data(6)), 16) +
                                                  Shift_Left(Unsigned_32(Block.Data(7)), 24) ;
                  begin
                     if not Is_Address_Mapped (IP, Pages) then
                        return Errors.Fail(Errors.INCOMPATIBLE, 
                                           "Vector table at " & Hex (Unsigned_32(MAIN_RAM_START)) &
                                           " is invalid: reset vector " & Hex (IP) &
                                           " is not in mapped memory");
                     end if;
                     if not Is_Address_Valid(SP - 4, Valid_Ranges.all) then
                        return Errors.Fail(Errors.INCOMPATIBLE, 
                                           "Vector table at " & Hex (Unsigned_32(MAIN_RAM_START)) &
                                           " is invalid: stack pointer " & Hex (SP) &
                                           " is not in RAM");
                     end if;
                  end;
               end;
            end if;
         end;
      end if;

      -- Write output file content
      declare
         Block : Uf2.uf2_block;
         Page_Num : Unsigned_32 := 0;
         Out_Stream : SIO.Stream_Access;
      begin
         Out_Stream := SIO.Stream (Out_File);

         Block.Magic_Start0 := Uf2.MAGIC_START0;
         Block.Magic_Start1 := Uf2.MAGIC_START1;
         Block.Flags        := Uf2.FLAG_FAMILY_ID_PRESENT;
         Block.Payload_Size := PAGE_SIZE;
         Block.Num_Blocks   := Unsigned_32(Pages.Length);
         Block.File_Size    := Uf2.RP2040_FAMILY_ID;
         Block.Magic_End    := Uf2.MAGIC_END;

         for Cursor in Pages.Iterate loop
            Block.Target_Addr := Unsigned_32(Page_Map.Key(Cursor));
            Block.Block_No    := Page_Num;
            Page_Num := Page_Num + 1;
            if Verbosity = 2 then
               Put_Line ("Page " & Unsigned_32'Image(Block.Block_No) & " / " & 
                                   Unsigned_32'Image(Block.Num_Blocks) & " " &
                                   Hex (Block.Target_Addr));
            end if;
            Block.Data := (others=>0);
 
            Ret_Code := Realize_Page(In_File, Pages(Cursor), Block);
            if Ret_Code /= Errors.NO_ERROR then
               return Ret_Code;
            end if;

            Uf2.Uf2_Block'Write(Out_Stream, Block);
         end loop;

      exception
         when others =>
            return Errors.Fail_Write_Error;
      end;

      --Put_Line("OK");
      return Errors.NO_ERROR;

   end Run;

   --**********************************
   function "=" (F1 : Fragment_Vector.Vector; F2 : Fragment_Vector.Vector) return Boolean is
      pragma Unreferenced (F1, F2);
   begin
      return True;   -- TODO : is this correct ?
   end "=";

   --**********************************
   function Read_And_Check_Elf32_Header (In_File   : SIO.File_Type;
                                         ElfHeader : in out Elf.Elf32_Header) return Exit_Status is
      In_Stream : SIO.Stream_Access;
   begin
      In_Stream := SIO.Stream (In_File);
      Elf.Elf32_Header'Read(In_Stream, ElfHeader);

      if ElfHeader.Common.Magic /= Elf.ELF_MAGIC then
         return Errors.Fail (Errors.FORMAT, "Not an ELF file");
      end if;

      if (ElfHeader.Common.Version /= 1) or (ElfHeader.Common.Version2 /= 1) then
         return Errors.Fail (Errors.FORMAT, "Unrecognized ELF version");
      end if;

      if (ElfHeader.Common.Arch_Class /= 1) or (ElfHeader.Common.endianness /= 1) then
         return Errors.Fail (Errors.INCOMPATIBLE, "Require 32 bit little-endian ELF");
      end if;

      if (ElfHeader.Eh_Size /= Elf.Elf32_Header'Size/8)  then
         return Errors.Fail (Errors.FORMAT, "Invalid ELF32 format");
      end if;

      if (ElfHeader.Common.Machine /= Elf.EM_ARM)  then
         return Errors.Fail (Errors.FORMAT, "Not an ARM executable");
      end if;

      if (ElfHeader.Common.Abi /= 0)  then
         return Errors.Fail (Errors.INCOMPATIBLE, "Unrecognized ABI");
      end if;

      if (ElfHeader.Flags and Elf.EF_ARM_ABI_FLOAT_HARD) /= 0 then
         return Errors.Fail (Errors.INCOMPATIBLE, "HARD-FLOAT not supported");
      end if;

      return Errors.NO_ERROR;

   exception
      when others =>
         return Errors.Fail (Errors.READ_FAILED, "Unable to read ELF header");
   end Read_And_Check_Elf32_Header;


   --**********************************
   function Read_And_Check_Elf32_Ph_Entries (In_File   : SIO.File_Type;
                                             ElfHeader : in Elf.Elf32_Header;
                                             Valid_Ranges : Address_Range_Array;
                                             Pages : in out Page_Map.Map) return Exit_Status is
      In_Stream : SIO.Stream_Access;
   begin
      In_Stream := SIO.Stream (In_File);

      if ElfHeader.Ph_Entry_Size /= Elf.Elf32_Ph_Entry'Size/8 then 
         return Errors.Fail(Errors.FORMAT, "Invalid ELF32 program header.");
      end if;
      if ElfHeader.Ph_Num /= 0 then
         if Verbosity = 2 then
            Put_Line ("ElfHeader.Ph_Num : " & Unsigned_16'Image(ElfHeader.Ph_Num));
         end if;

         declare
            subtype Entries_Array is Elf.Elf32_Ph_Entry_Array (1 .. Natural(ElfHeader.Ph_Num));
            Entries : Entries_Array;
            Continue : Boolean;
         begin
            if Verbosity = 2 then
               Put_Line ("ElfHeader.Ph_Offset : " & Unsigned_32'Image(ElfHeader.Ph_Offset));
            end if;

            SIO.Set_Index (In_File, SIO.Positive_Count(ElfHeader.Ph_Offset + 1));
            Entries_Array'Read(In_Stream, Entries);
            if Verbosity = 2 then
               Put_Line ("New Index : " & Integer'Image(Integer(SIO.Index(In_File))));
            end if;

            for Ph_Entry of Entries loop
               if Verbosity = 2 then
                  Put_Line ("Entry ");
                  Put ("  Entry_Type : " & Hex (Ph_Entry.Entry_Type));
                  Put_Line ("  Memsz      : " & Unsigned_32'Image (Ph_Entry.Memsz));
               end if;

               Continue := False;
               if (Ph_Entry.Entry_Type = Elf.PT_LOAD) and (Ph_Entry.Memsz /= 0) then
                  if Verbosity = 2 then
                     Put_Line ("    Entry LOAD");
                  end if;

                  declare
                     Mapped_Size : constant Unsigned_32 := Unsigned_32'Min(Ph_Entry.Filez, Ph_Entry.Memsz);
                     Addr_Range : Address_Range;
                     Ret_Code : Exit_Status;
                  begin
                     if Mapped_Size /= 0 then
                        Ret_Code := Check_Address_Range(Valid_Ranges, 
                                                        Ph_Entry.PAddr, 
                                                        Ph_Entry.VAddr, 
                                                        Mapped_Size, 
                                                        False, 
                                                        Addr_Range);
                        if Ret_Code /= Errors.NO_ERROR then
                           return Ret_Code;
                        end if;

                        -- We don't download uninitialized, generally it is BSS and should be zero-ed by crt0.S, or it may be COPY areas which are undefined
                        if Addr_Range.AR_Type /= CONTENTS then
                              if Verbosity = 2 then
                                 Put_Line("  ignored");
                              end if;
                              Continue := True;
                        end if;

                        if not Continue then
                           declare
                              Addr        : Unsigned_32 := Ph_Entry.PAddr;
                              File_Offset : Unsigned_32 := Ph_Entry.offset;
                              Remaining   : Unsigned_32 := Mapped_Size;
                           begin
                              while Remaining /= 0 loop
                                 declare
                                    Off   : constant Unsigned_32 := Addr and (PAGE_SIZE - 1);
                                    Len   : constant Unsigned_32 := Unsigned_32'Min(Remaining, PAGE_SIZE - Off);
                                 begin
                                    if not Pages.Contains(Addr - Off) then
                                       Pages.Include(Addr - Off, Fragment_Vector.Empty_Vector);
                                    end if;

                                    -- note if filesz is zero, we want zero init which is handled because the
                                    -- statement above creates an empty page fragment list
                                    -- check overlap with any existing fragments
                                    for Fragment of Pages(Addr - Off) loop
                                       if (Off < Fragment.Page_Offset + Fragment.NbBytes) /=
                                          ((Off + Len) <= Fragment.Page_Offset) then
                                          declare
                                             Error : Exit_Status;
                                          begin
                                             Error := Errors.Fail (Errors.FORMAT, "In memory segments overlap");
                                             pragma Unreferenced (Error);
                                          end;
                                       end if;
                                    end loop;
                                    Pages(Addr - Off).Append((File_Offset => File_Offset, 
                                                               Page_Offset => Off, 
                                                               NbBytes     => Len));

                                    Addr        := Addr + len;
                                    File_offset := File_Offset + len;
                                    Remaining   := Remaining - len;
                                 end;
                              end loop;
                           end;
                        end if;
                     end if;

                     if not Continue then
                        if Ph_Entry.Memsz > Ph_Entry.Filez then
                           -- We have some uninitialized data too
                           Ret_Code := Check_Address_Range(Valid_Ranges, 
                                                           Ph_Entry.PAddr + Ph_Entry.Filez, 
                                                           Ph_Entry.VAddr + Ph_Entry.Filez, 
                                                           Ph_Entry.Memsz - Ph_Entry.Filez, 
                                                           True,
                                                           Addr_Range);
                           if Ret_Code /= Errors.NO_ERROR then
                              return Ret_Code;
                           end if;
                        end if;
                     end if;
                  end;
               end if;
            end loop;
         end;
      end if;
      return Errors.NO_ERROR;
   end Read_And_Check_Elf32_Ph_Entries;

   --**********************************
   function Is_Address_Valid (Addr : Unsigned_32;
                              Valid_Ranges : Address_Range_Array) return Boolean is
   begin
      for Mem_Range of Valid_Ranges loop
         if (Mem_Range.From <= Addr) and (Addr < Mem_Range.To) then
            return True;
         end if;
      end loop;
      
      return False;
   end Is_Address_Valid;

   --**********************************
   function Is_Address_Initialized (Addr : Unsigned_32;
                                    Valid_Ranges : Address_Range_Array) return Boolean is
   begin
      for Mem_Range of Valid_Ranges loop
         if (Mem_Range.From <= Addr) and (Addr < Mem_Range.To) then
            return Mem_Range.AR_Type = CONTENTS;
         end if;
      end loop;
      
      return False;
   end Is_Address_Initialized;

   --**********************************
   function Is_Address_Mapped (Addr  : Unsigned_32;
                               Pages : Page_Map.Map) return Boolean is
      Page : constant Unsigned_32 := Addr and not(PAGE_SIZE - 1);
   begin
      if not Pages.Contains(Page) then
         return False;
      end if;
      -- TODO : check actual address within page
      return True;
   end Is_Address_Mapped;

   --**********************************
   function Check_Address_Range (Valid_Ranges  : Address_Range_Array;
                                 Addr          : Unsigned_32;
                                 VAddr         : Unsigned_32;
                                 Size          : Unsigned_32;
                                 Uninitialized : Boolean;
                                 Addr_Range    : out Address_Range) return Exit_Status is
   begin
      for Mem_Range of Valid_Ranges loop
         if (Mem_Range.From <= Addr) and (Addr + Size <= Mem_Range.To) then
            if (Mem_Range.AR_Type = NO_CONTENTS) and (not Uninitialized) then
                return Errors.Fail(Errors.INCOMPATIBLE, "ELF contains memory contents for uninitialized memory");
            end if;
            Addr_Range := Mem_Range;

            if verbosity >= 1 then
               if Uninitialized then
                  Put ("Uninitialized segment ");
               else
                  Put ("Mapped segment ");
               end if;
               Put_Line (Hex (Addr) & "->" & Hex (Addr + Size) &
                         "(" & Hex (VAddr) & "->" & Hex (VAddr + Size) & ")");
            end if;

            return Errors.NO_ERROR;
         end if;
      end loop;
      
      return Errors.Fail(Errors.INCOMPATIBLE, 
                         "Memory segment " & Hex (Addr) & 
                         "->" & Hex (Addr + Size) & 
                         " is outside of valid address range for device.");
   end ;

   --**********************************
   function Realize_Page (In_File   : SIO.File_Type;
                          Fragments : Fragment_Vector.Vector;
                          Block     : in out Uf2.uf2_block) return Exit_Status is
      In_Stream : SIO.Stream_Access;
   begin
      In_Stream := SIO.Stream (In_File);

      for Fragment of Fragments loop
         declare
            subtype t_Buffer is Uf2.Uf2_Block_Data_Array (1..Fragment.NbBytes);
            --Buffer : t_Buffer;
         begin
            SIO.Set_Index(In_File, SIO.Positive_Count(Fragment.File_Offset + 1)); -- Index is 1 based (not 0 based)
            --t_Buffer'Read(In_Stream, Buffer);
            --Block.Data(Fragment.Page_Offset+1 .. Fragment.Page_Offset+Fragment.NbBytes) := Buffer;
            t_Buffer'Read(In_Stream, Block.Data(Fragment.Page_Offset+1 .. Fragment.Page_Offset+Fragment.NbBytes));
         end;
      end loop;
      return Errors.NO_ERROR;
   end Realize_Page;

   --**********************************
   function Hex (Data : Unsigned_32) return String is
      Str : String(1..Data'Size/4) := (others=>'0');  -- 2 char per byte -> /4
      Index : Positive := Str'Length + 1;
      Data_c : Unsigned_32 := Data;
      Hex_Chars : constant String := "0123456789ABCDEF";
   begin
      while Data_c > 0 loop
         Index   := Index - 1;
         Str(Index) := Hex_Chars(Natural(Data_c mod 16) + 1);
         Data_c  := Data_c / 16;
      end loop;
      return Str;
   end Hex;

end elf2uf2;