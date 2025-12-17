# Tiny Autopilot Module Simulation

This is my Tiny Autopilot Simulation project, a small experiment while I’m learning Ada and exploring aerospace control systems. It’s a basic autopilot simulation that focuses on altitude, speed, and heading control, written with JSF-compliant principles (well, as much as I could manage—I’m still learning!).

The project currently features an Ada version following JSF-compliant standards, which was a challenge for me to figure out (I'm still learning!). But hey, I love the process, and this is just a stepping stone to better understanding both Ada and control systems in the aerospace field.

## What is JSF?
JSF stands for Joint Strike Fighter — it's a set of military aircraft software standards (think F-35) that emphasize things like safety, reliability, and determinism. These standards are inspired by Ada (which is another thing I’m currently obsessing over). Here are the key principles of JSF:

- No magic numbers — all constants should be clearly named (I’m not talking about the kind of magic that happens in my brain when I don’t know what I'm doing)
- Encapsulation — functions to keep control logic tidy and organized (no spaghetti code)
- Strong typing and modular design — to prevent that unpredictable, random behavior (and no, I’m not talking about the random things I do in life)
- Deterministic behavior — to make sure that the software does exactly the same thing every time it’s run (unlike my mind contemplating on what to eat today)

For this project, I’ve tried to follow JSF-inspired principles to write cleaner, more structured code with a focus on reliability and readability. I'm still learning, though. So if you see something questionable — well, that’s probably my "learning phase" showing.

## Project Features
- Randomized simulation of altitude, speed, and heading control.
- Altitude, speed, and heading trims based on the target values.
- Random disturbances to simulate things like turbulence or minor changes (because, you know, flying isn’t always smooth).🤷‍♂️
- JSF-compliant code: Clean, modular, and deterministic design inspired by JSF standards.

## How to Run

You’ll need GNAT to compile and run the Ada version of the project. Here’s how to get started:

1. Install GNAT if you don’t have it.
2. Compile and run the Ada version:

```bash
gnatmake Tiny_EGT.adb
./Tiny_EGT    # Windows: Tiny_EGT.exe
```

## Notes
- Educational Purpose: This is a small learning project to explore Ada, JSF standards, and aerospace control. I’m still figuring it out, but it’s been a fun challenge!
- JSF Standards: I’m following JSF-style principles to improve the design, but I’m still in the early stages of understanding them.
- Modular & Safe: The code uses clean, modular functions, and all constants are explicitly defined, following JSF-inspired principles.
- No Real Aircraft: This is a simulation only, so don’t expect real-world autopilot systems here. Just me learning and having fun with the process. ✈️