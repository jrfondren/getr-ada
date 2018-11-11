with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib;
with Ada.Command_Line; use Ada.Command_Line;
with System;
with Ada.Unchecked_Conversion;

procedure Getr is
   type Long is mod 2 ** 64;
   type Timeval is record
      sec : Long;
      usec : Long;
   end record;
   -- type Reserved is array(Integer range <>) of U64;
   -- use to pad RUsage if C lib's getrusage() has a 'reserved' member
   type RUsage is record
      User_Time : Timeval;
      System_Time : Timeval;
      Max_RSS : Long;
      Shared_RSS : Long;
      Unshared_RSS : Long;
      Unshared_Stack : Long;
      Minor_Faults : Long;
      Major_Faults : Long;
      Swaps : Long;
      In_Blocks : Long;
      Out_Blocks : Long;
      MSG_Send : Long;
      MSG_Recv : Long;
      Signal_Recv : Long;
      Vol_Context : Long;
      Invol_Context : Long;
      -- Reserved_Padding : Reserved(1 .. 10);
   end record;
   function getrusage(Who : Integer; Report : access RUsage) return Integer;
   pragma Import(C, getrusage, "getrusage");
   RUSAGE_CHILDREN : constant Integer := -1;

   function posix_spawn(
      pid : access Integer;
      path : Interfaces.C.Strings.chars_ptr;
      file_actions : System.Address;
      attrp : System.Address;
      argv : System.Address;
      envp : System.Address) return Integer; 
   pragma Import(C, posix_spawn, "posix_spawn");

   function waitpid(
      pid : Integer;
      wstatus : System.Address;
      options : Integer) return Integer;
   pragma Import(C, waitpid, "waitpid");

   Usage : aliased RUsage;
   SpawnN : Integer;

   function Image(J : in Long) return String is
      Str : constant String := Long'Image(J);
   begin
      if J < 0 then
         return Str;
      else
         return Str(2 .. Str'Length);
      end if;
   end Image;

   procedure PutTimes(Key : String; TV : Timeval) is
   begin
      Put_Line(Standard_Error, Key & Image(TV.sec) & " s, " & Image(TV.usec) & " us");
   end PutTimes;

   procedure PutTime(Key : String; MS : Long) is
   begin
      Put(Standard_Error, Key & Image(MS) & " ms (");
      Put(Standard_Error, Float(MS)/Float(SpawnN), Fore => 0, Aft => 3, Exp => 0);
      Put_Line(Standard_Error, " ms/per)");
   end PutTime;

   procedure PutUnit(Key : String; Val : Long; Unit : String) is
   begin
      Put_Line(Standard_Error, Key & Image(Val) & " " & Unit);
   end PutUnit;

   procedure PutVal(Key : String; Val : Long) is
   begin
      Put_Line(Standard_Error, Key & Image(Val));
   end PutVal;

   procedure Report_Usage is
      sec : Long;
      usec : Long;
      time_ms : Long;
   begin
      if 0 /= getrusage(RUSAGE_CHILDREN, Usage'Access) then
         Put_Line("Error: getrusage() failed");
      else
         sec := Usage.User_Time.sec + Usage.System_Time.sec;
         usec := Usage.User_Time.usec + Usage.System_Time.usec;
         time_ms := (sec*1000) + (usec/1000);
         PutTimes("User time      : ", Usage.User_Time);
         PutTimes("System time    : ", Usage.System_Time);
         PutTime( "Time           : ", time_ms);
         PutUnit( "Max RSS        : ", Usage.Max_RSS, "kB");
         PutVal(  "Page reclaims  : ", Usage.Minor_Faults);
         PutVal(  "Page faults    : ", Usage.Major_Faults);
         PutVal(  "Block inputs   : ", Usage.In_Blocks);
         PutVal(  "Block outputs  : ", Usage.Out_Blocks);
         PutVal("vol ctx switches   : ", Usage.Vol_Context);
         PutVal("invol ctx switches : ", Usage.Invol_Context);
      end if;
   end Report_Usage;

   procedure Spawns is
      environ : constant System.Address;
      pragma Import(C, environ, "environ");

      function getNull is new Ada.Unchecked_Conversion(System.Address, chars_ptr);
      NULLstr : constant chars_ptr := getNull(System.Null_Address);

      Child : aliased Integer;
      Command : chars_ptr := New_String(Argument(2));
      Args : array(1 .. Argument_Count) of chars_ptr := (1 => Command, others => NULLstr);
   begin
      for J in 2 .. Argument_Count loop
         Args(J-1) := New_String(Argument(J));
      end loop;

      for J in 1 .. SpawnN loop
         if 0 /= posix_spawn(Child'Access, Command, System.Null_Address, System.Null_Address, Args'Address, environ) then
            Put_Line(Standard_Error, "Error: posix_spawn() failed");
            GNAT.OS_Lib.OS_Exit(1);
         end if;
         if -1 = waitpid(Child, System.Null_Address, 0) then
            Put_Line(Standard_Error, "Error: waitpid failed");
            GNAT.OS_Lib.OS_Exit(1);
         end if;
      end loop;
   end Spawns;

begin
   if Argument_Count > 1 then
      SpawnN := Positive'Value(Argument(1));
      Spawns;
      Report_Usage;
   else
      Put_Line(Standard_Error, "usage: " & Command_Name & " <n> <command> [<args> ...]");
   end if;
end Getr;
