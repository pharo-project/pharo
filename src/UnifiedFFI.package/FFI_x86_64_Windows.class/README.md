I represent the X86_64 architecture, using the Windows X64 ABI.

Parameters passed in integer registers: RCX RDX R8 R9
Parameters passed in floating point registers: XMM0 to XMM3, inclusive
Shadow space for spilling registers: 32 bytes.
Structures are never split into registers with this ABI.

These ABI details are very important to keep in mind for receiving callbacks.