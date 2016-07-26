My instances (and subinstances) are objects suitable for execution by the virtual machine. My subclasses and I have a specific layout so the instances intermix both indexable pointer fields and indexable integer fields.

	
The current format of a CompiledCode is as follows:

	header (4 bytes)
	literals (4 bytes each)
	bytecodes  (variable)
	trailer (variable)

The header describes the compiled code. It's a small integer with the following format: 

sign bit 	1 bit: 	if set, the method is encoded in the SecondaryBytecodeSet, else in the PrimaryBytecodeSet (See class variables) 
(index 0)	15 bits:	number of literals
(index 16)	1 bit:	requires counters (in the sista JIT, methods marked this way cannot trip)
(index 17)	1 bit:	whether a large frame size is needed
(index 18)	6 bits:	number of temporary variables
(index 24)	4 bits:	number of arguments to the method
(index 28)	1 bit:	has primitive
(index 29)	1 bit:	flag bit, ignored by the VM

The trailer encodes how to fetch the method's sources. See CompiledMethodTrailer.