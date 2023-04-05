# chip8
Work in progress chip8 emulator using F# that I haven't had a chance to flesh out.

Immutable data types were used because I wanted to make my RAM earn its keep (and maybe some multiplayer with rollback in the future if I can figure out a good way to handle the shared keypad for two players).


# Future goals
- Add a visual decay (trails) system to help mask the inherent flicker of chip8 ROMs
- Compare an imperative approach with the naive immutable version
  - If there's not a significant difference, run enough instances of the immutable one in parallel 'til my machine gets angry
- Replace the object/record implementation with a struct-based one
