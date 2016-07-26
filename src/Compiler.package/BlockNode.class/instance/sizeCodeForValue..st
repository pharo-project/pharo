code generation (closures)
sizeCodeForValue: encoder
	"Compute the size for the creation of the block and its code."
	"If we have the closure bytecodes constructClosureCreationNode: will note
	 the copied values in the copiedValues inst var and answer #pushCopiedValues."
	closureCreationNode := self constructClosureCreationNode: encoder.
	"Remember size of body for emit time so we know the size of the jump around it."
	size := self sizeCodeForEvaluatedClosureValue: encoder.
	^encoder supportsClosureOpcodes
		ifTrue:
			[(copiedValues inject: 0 into: [:sum :node| sum + (node sizeCodeForValue: encoder)])
			 + (encoder sizePushClosureCopyNumCopiedValues: copiedValues size numArgs: arguments size jumpSize: size)
			 + size]
		ifFalse:
			["closureCreationSupportNode is send closureCopy:copiedValues:"
			(closureCreationNode sizeCodeForValue: encoder)
			 + (encoder sizeJumpLong: size)
			 + size]