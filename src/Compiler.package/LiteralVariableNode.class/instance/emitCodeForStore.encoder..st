code generation
emitCodeForStore: stack encoder: encoder
	writeNode ifNil: [^encoder genStoreLiteralVar: index].
	"THIS IS WRONG!! THE VALUE IS LOST FROM THE STACK!!
	 The various value: methods on Association ReadOnlyVariableBinding
	 etc _do not_ return the value assigned; they return the receiver."
	"Should generate something more like
		push expr
		push lit
		push temp (index of expr)
		send value:
		pop
	or use e.g. valueForStore:"
	self flag: #bogus.
	writeNode
		emitCode: stack
		args: 1
		encoder: encoder
		super: false