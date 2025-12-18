import random

# ==============================
# Constants (No magic numbers)
# ==============================
Base_Altitude = 10000
Base_Speed = 300
Base_Heading = 0
Max_Altitude_Delta = 1000
Max_Speed_Delta = 10
Max_Heading_Delta = 10
Sudden_Range = 10
Disturbance_Chance = 5

# ==============================
# Variables
# ==============================
Altitude = Base_Altitude
Speed = Base_Speed
Heading = Base_Heading
Altitude_Trim = 0
Speed_Trim = 0
Heading_Trim = 0

# ==============================
# Control Logic Functions
# ==============================

def control_altitude(current_altitude):
    global Altitude
    if current_altitude < Base_Altitude:
        print("Increasing altitude")
        return 100  # Ascend
    elif current_altitude > Base_Altitude:
        print("Decreasing altitude")
        return -100  # Descend
    else:
        print("Altitude stable")
        return 0  # Maintain Altitude

def control_speed(current_speed):
    global Speed
    if current_speed < Base_Speed:
        print("Speeding up")
        return 5  # Speed up
    elif current_speed > Base_Speed:
        print("Slowing down")
        return -5  # Slow down
    else:
        print("Speed stable")
        return 0  # Maintain speed

def control_heading(current_heading):
    global Heading
    if current_heading < Base_Heading:
        print("Turning right")
        return 5  # Turn right
    elif current_heading > Base_Heading:
        print("Turning left")
        return -5  # Turn left
    else:
        print("Heading stable")
        return 0  # Maintain heading

# ==============================
# Disturbance Types
# ==============================

# Standard Disturbances
def apply_altitude_disturbance():
    global Altitude
    Altitude += random.randint(-Max_Altitude_Delta//2, Max_Altitude_Delta//2)

def apply_speed_disturbance():
    global Speed
    Speed += random.randint(-Max_Speed_Delta//2, Max_Speed_Delta//2)

def apply_heading_disturbance():
    global Heading
    Heading += random.randint(-Max_Heading_Delta//2, Max_Heading_Delta//2)

# New Disturbance Types
def apply_wind_gust():
    global Speed
    wind_change = random.randint(-Max_Speed_Delta, Max_Speed_Delta)
    if wind_change > 0:
        print("Wind gust increasing speed")
    else:
        print("Wind gust decreasing speed")
    Speed += wind_change

def apply_engine_failure():
    global Speed
    print("Engine failure: Speed reduced")
    Speed -= 50  # Significant drop in speed

def apply_pressure_drop():
    global Altitude
    print("Altitude pressure drop: Rapid descend")
    Altitude -= 2000  # Rapid descent

def apply_electrical_failure():
    global Heading
    print("Electrical failure: Heading frozen")
    Heading = Base_Heading

def apply_weather_turbulence():
    global Altitude, Speed, Heading
    print("Weather turbulence affecting all parameters")
    Altitude += random.randint(-Max_Altitude_Delta//2, Max_Altitude_Delta//2)
    Speed += random.randint(-Max_Speed_Delta//2, Max_Speed_Delta//2)
    Heading += random.randint(-Max_Heading_Delta//2, Max_Heading_Delta//2)

# ==============================
# Random Disturbance Logic
# ==============================

def apply_random_disturbance():
    if random.randint(1, Sudden_Range) == Disturbance_Chance:
        disturbance_type = random.randint(0, 7)
        if disturbance_type == 0:
            apply_altitude_disturbance()
        elif disturbance_type == 1:
            apply_speed_disturbance()
        elif disturbance_type == 2:
            apply_heading_disturbance()
        elif disturbance_type == 3:
            apply_wind_gust()
        elif disturbance_type == 4:
            apply_engine_failure()
        elif disturbance_type == 5:
            apply_pressure_drop()
        elif disturbance_type == 6:
            apply_electrical_failure()
        elif disturbance_type == 7:
            apply_weather_turbulence()

# ==============================
# Check Edge Cases
# ==============================

def check_edge_cases():
    global Altitude, Speed, Heading
    if Altitude < 0:
        print("Error: Altitude below sea level. Setting to 0.")
        Altitude = 0
    if Heading < 0:
        print("Error: Heading below 0 degrees. Setting to 0.")
        Heading = 0
    elif Heading >= 360:
        print("Error: Heading exceeds 360 degrees. Setting to 359.")
        Heading = 359
    if Speed < 0:
        print("Error: Speed below 0. Setting to 0.")
        Speed = 0

# ==============================
# Log Flight Status
# ==============================

def log_flight_status():
    print(f"Altitude: {Altitude} | Speed: {Speed} | Heading: {Heading}")

# ==============================
# Simulation Loop
# ==============================

print("=== Tiny Autopilot Simulation ===")

for i in range(20):
    # Random disturbance chance
    apply_random_disturbance()

    # Control logic: Adjust altitude, speed, and heading
    Altitude_Trim = control_altitude(Altitude)
    Speed_Trim = control_speed(Speed)
    Heading_Trim = control_heading(Heading)

    # Apply adjustments
    Altitude += Altitude_Trim
    Speed += Speed_Trim
    Heading += Heading_Trim

    # Check edge cases
    check_edge_cases()

    # Log flight status
    log_flight_status()

    print()

print("=== Autopilot Simulation Complete ===")