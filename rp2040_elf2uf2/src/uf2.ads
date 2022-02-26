--
--  Copyright 2022 (C) Nicolas Pinault (aka DrPi)
--
--  SPDX-License-Identifier: BSD-3-Clause
--

with Interfaces; use Interfaces;

package Uf2 is

   MAGIC_START0 : constant := 16#0A324655#;
   MAGIC_START1 : constant := 16#9E5D5157#;
   MAGIC_END    : constant := 16#0AB16F30#;

   FLAG_NOT_MAIN_FLASH    : constant := 16#00000001#;
   FLAG_FILE_CONTAINER    : constant := 16#00001000#;
   FLAG_FAMILY_ID_PRESENT : constant := 16#00002000#;
   FLAG_MD5_PRESENT       : constant := 16#00004000#;

   RP2040_FAMILY_ID : constant := 16#e48bff56#;

   type Uf2_Block_Data_Array is array (Unsigned_32 range <>) of Unsigned_8;

   type Uf2_Block is record
      -- 32 byte header
      Magic_Start0 : Unsigned_32;-- := MAGIC_START0;
      Magic_Start1 : Unsigned_32;-- := MAGIC_START1;
      Flags        : Unsigned_32;
      Target_Addr  : Unsigned_32;
      Payload_Size : Unsigned_32;
      Block_No     : Unsigned_32;
      Num_Blocks   : Unsigned_32;
      File_Size    : Unsigned_32;     -- or familyID;
      Data         : Uf2_Block_Data_Array (1 .. 476);
      Magic_End    : Unsigned_32;-- := MAGIC_END;
   end record
      with Size => 512*8;

   for Uf2_Block use record
      Magic_Start0 at   0 range 0 .. 31;
      Magic_Start1 at   4 range 0 .. 31;
      Flags        at   8 range 0 .. 31;
      Target_Addr  at  12 range 0 .. 31;
      Payload_Size at  16 range 0 .. 31;
      Block_No     at  20 range 0 .. 31;
      Num_Blocks   at  24 range 0 .. 31;
      File_Size    at  28 range 0 .. 31;
      Data         at  32 range 0 .. 476*8-1;
      Magic_End    at 508 range 0 .. 31;
   end record;

end Uf2;