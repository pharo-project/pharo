Instance variables:
	sender: <Context|nil> context that invoked this context
	pc: <SmallInteger> (pc = program counter) offset of the bytecode instruction currently executed

My instances can interpret the byte-encoded Smalltalk instruction set. They maintain a program counter (pc) for streaming through CompiledMethods. My subclasses are Contexts, which inherit this capability. They store the return pointer in the instance variable sender, and the current position in their method in the instance variable pc. For other users, sender can hold a method to be similarly interpreted. The unclean re-use of sender to hold the method was to avoid a trivial subclass for the stand-alone scanning function.