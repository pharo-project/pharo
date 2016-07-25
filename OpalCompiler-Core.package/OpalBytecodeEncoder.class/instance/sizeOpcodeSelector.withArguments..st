opcode sizing
sizeOpcodeSelector: genSelector withArguments: args
	stream := self.
	position := 0.
	self perform: genSelector withArguments: args.
	^position