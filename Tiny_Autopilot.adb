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
         return 100;    -- Ascend
      elsif Current_Altitude > Base_Altitude then
         return -100;   -- Descend
      else
         return 0;      -- Maintain Altitude
      end if;
   end Control_Altitude;

   function Control_Speed(Current_Speed: Integer) return Integer is
   begin
      if Current_Speed < Base_Speed then
         return 5;      -- Speed up
      elsif Current_Speed > Base_Speed then
         return -5;     -- Slow down
      else
         return 0;      -- Maintain speed
      end if;
   end Control_Speed;

   function Control_Heading(Current_Heading: Integer) return Integer is
   begin
      if Current_Heading < Base_Heading then
         return 5;      -- Turn right
      elsif Current_Heading > Base_Heading then
         return -5;     -- Turn left
      else
         return 0;      -- Maintain heading
      end if;
   end Control_Heading;

begin
   Put_Line("=== Tiny Autopilot Simulation ===");

   -- Initialize random generator
   Gen := Random_Pkg.Generator;
   Random_Pkg.Reset(Gen);

   -- Simulation loop (Fixed number of steps)
   for I in 1 .. 20 loop
      -- Random disturbance chance
      Rand := Random_Pkg.Random(Gen);
      declare
         Disturbance : Integer := Integer(Rand * Float(Sudden_Range));
      begin
         if Disturbance = Disturbance_Chance then
            -- Apply random disturbance to one of the parameters
            case Integer(Rand * 3) is
               when 0 =>
                  Altitude := Altitude + (Integer(Rand * Float(Max_Altitude_Delta)) - Max_Altitude_Delta/2);
               when 1 =>
                  Speed := Speed + (Integer(Rand * Float(Max_Speed_Delta)) - Max_Speed_Delta/2);
               when 2 =>
                  Heading := Heading + (Integer(Rand * Float(Max_Heading_Delta)) - Max_Heading_Delta/2);
               when others =>
                  null;
            end case;
         end if;
      end;

      -- Control logic: Adjust altitude, speed, and heading towards the targets
      Altitude_Trim := Control_Altitude (Altitude);
      Speed_Trim := Control_Speed (Speed);
      Heading_Trim := Control_Heading (Heading);

      -- Apply the adjustments
      Altitude := Altitude + Altitude_Trim;
      Speed    := Speed + Speed_Trim;
      Heading  := Heading + Heading_Trim;

      -- Display current values and adjustments
      Put_Line("Altitude: " & Integer'Image(Altitude) &
               " | Speed: " & Integer'Image(Speed) &
               " | Heading: " & Integer'Image(Heading));
      Put_Line("Altitude Trim: " & Integer'Image(Altitude_Trim) &
               " | Speed Trim: " & Integer'Image(Speed_Trim) &
               " | Heading Trim: " & Integer'Image(Heading_Trim));

      -- Display simulation status
      -- Altitude
      if Altitude = Base_Altitude then
         Put_Line("Altitude Stabilized.");
      elsif Altitude < Base_Altitude then
         Put_Line("Climbing.");
      else
         Put_Line("Descending.");
      end if;

      -- Speed
      if Speed = Base_Speed then
         Put_Line("Speed Stabilized.");
      elsif Speed < Base_Speed then
         Put_Line("Accelerating.");
      else
         Put_Line("Decelerating.");
      end if;

      -- Heading
      if Heading = Base_Heading then
         Put_Line("Heading Stabilized.");
      elsif Heading < Base_Heading then
         Put_Line("Turning right.");
      else
         Put_Line("Turning left.");
      end if;

      New_Line;
   end loop;

   Put_Line("=== Autopilot Simulation Complete ===");

end Tiny_Autopilot;