code generation (closures)
emitCodeForValue: stack encoder: encoder
	"if not supportsClosureOpcodes closureCreationSupportNode is the
	 node for thisContext closureCopy: numArgs [ copiedValues: { values } ]"
	encoder supportsClosureOpcodes
		ifTrue:
			[copiedValues do:
				[:copiedValue| copiedValue emitCodeForValue: stack encoder: encoder].
			 closureCreationNode pc: encoder methodStreamPosition + 1.
			 encoder
				genPushClosureCopyNumCopiedValues: copiedValues size
				numArgs: arguments size
				jumpSize: size.
			 stack
				pop: copiedValues size;
				push: 1]
		ifFalse:
			[closureCreationNode emitCodeForValue: stack encoder: encoder.
			 encoder genJumpLong: size]. "Force a two byte jump."
	"Emit the body of the block"
	self emitCodeForEvaluatedClosureValue: stack encoder: encoder