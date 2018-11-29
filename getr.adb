with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Float_Text_IO;    use Ada.Float_Text_IO;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib;
with Ada.Command_Line;     use Ada.Command_Line;
with System;
with Ada.Unchecked_Conversion;

procedure Getr is
   type Long is mod 2**64;
   type TimeVal is record
      sec  : Long;
      usec : Long;
   end record;
   --  type Reserved is array(Integer range <>) of U64;
   --  use to pad RUsage if C lib's getrusage() has a 'reserved' member
   type RUsage is record
      User_Time      : TimeVal;
      System_Time    : TimeVal;
      Max_RSS        : Long;
      Shared_RSS     : Long;
      Unshared_RSS   : Long;
      Unshared_Stack : Long;
      Minor_Faults   : Long;
      Major_Faults   : Long;
      Swaps          : Long;
      In_Blocks      : Long;
      Out_Blocks     : Long;
      Msg_Send       : Long;
      Msg_Recv       : Long;
      Signal_Recv    : Long;
      Vol_Context    : Long;
      Invol_Context  : Long;
      --  Reserved_Padding : Reserved(1 .. 10);
   end record;
   function getrusage (Who : Integer; Report : access RUsage) return Integer;
   pragma Import (C, getrusage, "getrusage");
   RUSAGE_CHILDREN : constant Integer := -1;

   function posix_spawn
     (Pid          : access Integer;
      Path         : Interfaces.C.Strings.chars_ptr;
      File_Actions : System.Address;
      Attrp        : System.Address;
      Argv         : System.Address;
      Envp         : System.Address)
      return Integer;
   pragma Import (C, posix_spawn, "posix_spawn");

   function waitpid
     (Pid     : Integer;
      Wstatus : System.Address;
      Options : Integer)
      return Integer;
   pragma Import (C, waitpid, "waitpid");

   Usage  : aliased RUsage;
   SpawnN : Integer;

   function Image (J : in Long) return String is
      Str : constant String := Long'Image (J);
   begin
      return Str (2 .. Str'Length);
   end Image;

   procedure Put_Times (Key : String; Tv : TimeVal) is
   begin
      Put_Line
        (Standard_Error,
         Key & Image (Tv.sec) & " s, " & Image (Tv.usec) & " us");
   end Put_Times;

   procedure Put_Time (Key : String; MS : Long) is
   begin
      Put (Standard_Error, Key & Image (MS) & " ms (");
      Put
        (Standard_Error,
         Float (MS) / Float (SpawnN),
         Fore => 0,
         Aft  => 3,
         Exp  => 0);
      Put_Line (Standard_Error, " ms/per)");
   end Put_Time;

   procedure Put_Unit (Key : String; Val : Long; Unit : String) is
   begin
      Put_Line (Standard_Error, Key & Image (Val) & " " & Unit);
   end Put_Unit;

   procedure Put_Val (Key : String; Val : Long) is
   begin
      Put_Line (Standard_Error, Key & Image (Val));
   end Put_Val;

   procedure Report_Usage is
      sec     : Long;
      usec    : Long;
      Time_MS : Long;
   begin
      if 0 /= getrusage (RUSAGE_CHILDREN, Usage'Access) then
         Put_Line ("Error: getrusage() failed");
      else
         sec     := Usage.User_Time.sec + Usage.System_Time.sec;
         usec    := Usage.User_Time.usec + Usage.System_Time.usec;
         Time_MS := (sec * 1000) + (usec / 1000);
         Put_Times ("User time      : ", Usage.User_Time);
         Put_Times ("System time    : ", Usage.System_Time);
         Put_Time ("Time           : ", Time_MS);
         Put_Unit ("Max RSS        : ", Usage.Max_RSS, "kB");
         Put_Val ("Page reclaims  : ", Usage.Minor_Faults);
         Put_Val ("Page faults    : ", Usage.Major_Faults);
         Put_Val ("Block inputs   : ", Usage.In_Blocks);
         Put_Val ("Block outputs  : ", Usage.Out_Blocks);
         Put_Val ("vol ctx switches   : ", Usage.Vol_Context);
         Put_Val ("invol ctx switches : ", Usage.Invol_Context);
      end if;
   end Report_Usage;

   procedure Spawns is
      Environ : constant System.Address;
      pragma Import (C, Environ, "environ");

      function Get_Null is new Ada.Unchecked_Conversion (System.Address,
         chars_ptr);
      Nullstr : constant chars_ptr := Get_Null (System.Null_Address);

      Child   : aliased Integer;
      Command : chars_ptr := New_String (Argument (2));
      Args    : array (1 .. Argument_Count) of chars_ptr :=
        (1 => Command, others => Nullstr);
   begin
      for J in 2 .. Argument_Count loop
         Args (J - 1) := New_String (Argument (J));
      end loop;

      for J in 1 .. SpawnN loop
         if 0 /=
           posix_spawn
             (Child'Access, Command, System.Null_Address, System.Null_Address,
              Args'Address, Environ)
         then
            Put_Line (Standard_Error, "Error: posix_spawn() failed");
            GNAT.OS_Lib.OS_Exit (1);
         end if;
         if -1 = waitpid (Child, System.Null_Address, 0) then
            Put_Line (Standard_Error, "Error: waitpid failed");
            GNAT.OS_Lib.OS_Exit (1);
         end if;
      end loop;

      Free (Command);
      for J in Args'Range loop
         Free (Args (J));
      end loop;
   end Spawns;

begin
   if Argument_Count > 1 then
      SpawnN := Positive'Value (Argument (1));
      Spawns;
      Report_Usage;
   else
      Put_Line
        (Standard_Error,
         "usage: " & Command_Name & " <n> <command> [<args> ...]");
      GNAT.OS_Lib.OS_Exit (1);
   end if;
end Getr;
