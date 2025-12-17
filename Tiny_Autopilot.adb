with Ada.Text_IO;        use Ada.Text_IO;
with Ada.Numerics.Float_Random;
with Ada.Containers.Vectors;

procedure Tiny_Autopilot is

   -- ===============================
   -- Constants (No magic numbers)
   -- ===============================

   Base_Altitude        : constant Integer   := 10000;
   Base_Speed           : constant Integer   := 300;
   Base_Heading         : constant Integer   := 0;
   Max_Altitude_Delta   : constant Integer   := 1000;
   Max_Speed_Delta      : constant Integer   := 10;
   Max_Heading_Delta    : constant Integer   := 10;
   Sudden_Range         : constant Integer   := 10;
   Disturbance_Chance   : constant Integer   := 5;

   -- ===============================
   -- Variables
   -- ===============================
   Altitude       : Integer   := Base_Altitude;
   Speed          : Integer   := Base_Speed;
   Heading        : Integer   := Base_Heading;
   Altitude_Trim  : Integer   := 0;
   Speed_Trim     : Integer   := 0;
   Heading_Trim   : Integer   := 0;

   -- Random number generator
   package Random_Pkg is new Ada.Numerics.Float_Random;
   Gen   : Random_Pkg.Generator;
   Rand  : Float;

   -- ===============================
   -- Control Logic Functions
   -- ===============================

   function Control_Altitude(Current_Altitude: Integer) return Integer is
   begin
      if Current_Altitude < Base_Altitude then
         Put_Line("Increasing altitude");
         return 100;    -- Ascend
      elsif Current_Altitude > Base_Altitude then
         Put_Line("Decreasing altitude");
         return -100;   -- Descend
      else
         Put_Line("Altitude stable");
         return 0;      -- Maintain Altitude
      end if;
   end Control_Altitude;

   function Control_Speed(Current_Speed: Integer) return Integer is
   begin
      if Current_Speed < Base_Speed then
         Put_Line("Speeding up");
         return 5;      -- Speed up
      elsif Current_Speed > Base_Speed then
         Put_Line("Slowing down");
         return -5;     -- Slow down
      else
         Put_Line("Speed stable");
         return 0;      -- Maintain speed
      end if;
   end Control_Speed;

   function Control_Heading(Current_Heading: Integer) return Integer is
   begin
      if Current_Heading < Base_Heading then
         Put_Line("Turning right");
         return 5;      -- Turn right
      elsif Current_Heading > Base_Heading then
         Put_Line("Turning left");
         return -5;     -- Turn left
      else
         Put_Line("Heading stable");
         return 0;      -- Maintain heading
      end if;
   end Control_Heading;

   procedure Apply_Random_Disturbance is
   Disturbance : Integer := Integer(Rand * Float(Sudden_Range));
   begin
      if Disturbance = Disturbance_Chance then
         case Integer(Rand * 3) is
            when 0 => Apply_Altitude_Disturbance;
            when 1 => Apply_Speed_Disturbance;
            when 2 => Apply_Heading_Disturbance;
            when others => null;
         end case;
      end if;
   end Apply_Random_Disturbance;

   procedure Apply_Altitude_Disturbance is
   begin
      Altitude := Altitude + (Integer(Rand * Float(Max_Altitude_Delta)) - Max_Altitude_Delta/2);
   end Apply_Altitude_Disturbance;

   procedure Apply_Speed_Disturbance is
   begin
      Speed := Speed + (Integer(Rand * Float(Max_Speed_Delta)) - Max_Speed_Delta/2);
   end Apply_Speed_Disturbance;

   procedure Apply_Heading_Disturbance is
   begin
      Heading := Heading + (Integer(Rand * Float(Max_Heading_Delta)) - Max_Heading_Delta/2);
   end Apply_Heading_Disturbance;

    procedure Log_Flight_Status(Altitude, Speed, Heading: Integer) is
   begin
      Put_Line("Altitude: " & Integer'Image(Altitude) &
               " | Speed: " & Integer'Image(Speed) &
               " | Heading: " & Integer'Image(Heading));
   end Log_Flight_Status;

   procedure Check_Edge_Cases is
   begin
      -- Ensure altitude doesn't go below sea level
      if Altitude < 0 then
         Put_Line("Error: Altitude below sea level. Setting to 0.");
         Altitude := 0;
      end if;

      -- Ensure heading is between 0 and 360 degrees
      if Heading < 0 then
         Put_Line("Error: Heading below 0 degrees. Setting to 0.");
         Heading := 0;
      elsif Heading >= 360 then
         Put_Line("Error: Heading exceeds 360 degrees. Setting to 359.");
         Heading := 359;
      end if;

      -- Ensure speed is non-negative
      if Speed < 0 then
         Put_Line("Error: Speed below 0. Setting to 0.");
         Speed := 0;
      end if;
   end Check_Edge_Cases;


begin
   Put_Line("=== Tiny Autopilot Simulation ===");

   -- Initialize random generator
   Gen := Random_Pkg.Generator;
   Random_Pkg.Reset(Gen);

   -- Simulation loop (Fixed number of steps)
   for I in 1 .. 20 loop
      -- Random disturbance chance
      Rand := Random_Pkg.Random(Gen);
      Apply_Random_Disturbance;

      -- Control logic: Adjust altitude, speed, and heading towards the targets
      Altitude_Trim := Control_Altitude (Altitude);
      Speed_Trim := Control_Speed (Speed);
      Heading_Trim := Control_Heading (Heading);

      -- Apply the adjustments
      Altitude := Altitude + Altitude_Trim;
      Speed    := Speed + Speed_Trim;
      Heading  := Heading + Heading_Trim;

      -- Check for edge cases
      Check_Edge_Cases;

      -- Log flight status
      Log_Flight_Status(Altitude, Speed, Heading);

      New_Line;
   end loop;

   Put_Line("=== Autopilot Simulation Complete ===");

end Tiny_Autopilot;