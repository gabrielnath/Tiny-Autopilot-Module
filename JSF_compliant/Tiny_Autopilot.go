package main

import (
	"fmt"
	"math/rand"
	"time"
)

// ==============================
// Constants (No magic numbers)
// ==============================
const (
	BaseAltitude       = 10000
	BaseSpeed          = 300
	BaseHeading        = 0
	MaxAltitudeDelta   = 1000
	MaxSpeedDelta      = 10
	MaxHeadingDelta    = 10
	SuddenRange        = 10
	DisturbanceChance  = 5
)

// ==============================
// Variables
// ==============================
var (
	Altitude      = BaseAltitude
	Speed         = BaseSpeed
	Heading       = BaseHeading
	AltitudeTrim  = 0
	SpeedTrim     = 0
	HeadingTrim   = 0
)

// ==============================
// Control Logic Functions
// ==============================

func controlAltitude(currentAltitude int) int {
	if currentAltitude < BaseAltitude {
		fmt.Println("Increasing altitude")
		return 100 // Ascend
	} else if currentAltitude > BaseAltitude {
		fmt.Println("Decreasing altitude")
		return -100 // Descend
	} else {
		fmt.Println("Altitude stable")
		return 0 // Maintain Altitude
	}
}

func controlSpeed(currentSpeed int) int {
	if currentSpeed < BaseSpeed {
		fmt.Println("Speeding up")
		return 5 // Speed up
	} else if currentSpeed > BaseSpeed {
		fmt.Println("Slowing down")
		return -5 // Slow down
	} else {
		fmt.Println("Speed stable")
		return 0 // Maintain speed
	}
}

func controlHeading(currentHeading int) int {
	if currentHeading < BaseHeading {
		fmt.Println("Turning right")
		return 5 // Turn right
	} else if currentHeading > BaseHeading {
		fmt.Println("Turning left")
		return -5 // Turn left
	} else {
		fmt.Println("Heading stable")
		return 0 // Maintain heading
	}
}

// ==============================
// Disturbance Types
// ==============================

func applyAltitudeDisturbance() {
	Altitude += rand.Intn(MaxAltitudeDelta) - MaxAltitudeDelta/2
}

func applySpeedDisturbance() {
	Speed += rand.Intn(MaxSpeedDelta) - MaxSpeedDelta/2
}

func applyHeadingDisturbance() {
	Heading += rand.Intn(MaxHeadingDelta) - MaxHeadingDelta/2
}

func applyWindGust() {
	windChange := rand.Intn(MaxSpeedDelta)
	if windChange > 0 {
		fmt.Println("Wind gust increasing speed")
	} else {
		fmt.Println("Wind gust decreasing speed")
	}
	Speed += windChange
}

func applyEngineFailure() {
	fmt.Println("Engine failure: Speed reduced")
	Speed -= 50 // Significant drop in speed
}

func applyPressureDrop() {
	fmt.Println("Altitude pressure drop: Rapid descend")
	Altitude -= 2000 // Rapid descent
}

func applyElectricalFailure() {
	fmt.Println("Electrical failure: Heading frozen")
	Heading = BaseHeading
}

func applyWeatherTurbulence() {
	fmt.Println("Weather turbulence affecting all parameters")
	Altitude += rand.Intn(MaxAltitudeDelta) - MaxAltitudeDelta/2
	Speed += rand.Intn(MaxSpeedDelta) - MaxSpeedDelta/2
	Heading += rand.Intn(MaxHeadingDelta) - MaxHeadingDelta/2
}

// ==============================
// Random Disturbance Logic
// ==============================

func applyRandomDisturbance() {
	if rand.Intn(SuddenRange) == DisturbanceChance {
		disturbanceType := rand.Intn(8)
		switch disturbanceType {
		case 0:
			applyAltitudeDisturbance()
		case 1:
			applySpeedDisturbance()
		case 2:
			applyHeadingDisturbance()
		case 3:
			applyWindGust()
		case 4:
			applyEngineFailure()
		case 5:
			applyPressureDrop()
		case 6:
			applyElectricalFailure()
		case 7:
			applyWeatherTurbulence()
		}
	}
}

// ==============================
// Check Edge Cases
// ==============================

func checkEdgeCases() {
	if Altitude < 0 {
		fmt.Println("Error: Altitude below sea level. Setting to 0.")
		Altitude = 0
	}
	if Heading < 0 {
		fmt.Println("Error: Heading below 0 degrees. Setting to 0.")
		Heading = 0
	} else if Heading >= 360 {
		fmt.Println("Error: Heading exceeds 360 degrees. Setting to 359.")
		Heading = 359
	}
	if Speed < 0 {
		fmt.Println("Error: Speed below 0. Setting to 0.")
		Speed = 0
	}
}

// ==============================
// Log Flight Status
// ==============================

func logFlightStatus() {
	fmt.Printf("Altitude: %d | Speed: %d | Heading: %d\n\n", Altitude, Speed, Heading)
}

// ==============================
// Simulation Loop
// ==============================

func main() {
	rand.Seed(time.Now().UnixNano())

	fmt.Println("=== Tiny Autopilot Simulation ===")

	for i := 0; i < 20; i++ {
		applyRandomDisturbance()

		// Control logic: Adjust altitude, speed, and heading
		AltitudeTrim = controlAltitude(Altitude)
		SpeedTrim = controlSpeed(Speed)
		HeadingTrim = controlHeading(Heading)

		// Apply adjustments
		Altitude += AltitudeTrim
		Speed += SpeedTrim
		Heading += HeadingTrim

		// Check edge cases
		checkEdgeCases()

		// Log flight status
		logFlightStatus()
	}

	fmt.Println("=== Autopilot Simulation Complete ===")
}