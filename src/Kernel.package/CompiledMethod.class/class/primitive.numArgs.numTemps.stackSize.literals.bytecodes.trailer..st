instance creation
primitive: primNum numArgs: numArgs numTemps: numTemps stackSize: stackSize literals: literals bytecodes: bytecodes trailer: trailerBytes
	"Create method with given attributes.  numTemps includes numArgs.  stackSize does not include numTemps."

	| compiledMethod |
	compiledMethod := self
		newBytes: bytecodes size
		trailerBytes: trailerBytes 
		nArgs: numArgs
		nTemps: numTemps
		nStack: 0
		nLits: literals size
		primitive: primNum.
	(WriteStream with: compiledMethod)
		position: compiledMethod initialPC - 1;
		nextPutAll: bytecodes.
	literals withIndexDo: [:obj :i | compiledMethod literalAt: i put: obj].
	compiledMethod needsFrameSize: stackSize.
	^ compiledMethod