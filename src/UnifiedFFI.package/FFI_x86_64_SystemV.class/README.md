I represent the X86_64 architecture, using the SystemV ABI.

Parameters passed in integer registers: RDI RSI RDX RCX R8 R9
Parameters passed in floating point registers: XMM0 to XXM7, inclusive
Some small structures are split and passed in registers, according to an algorithm described in the ABI.

These ABI details are very important to keep in mind for receiving callbacks.