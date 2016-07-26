control flow instructions
interpret: endPC nextLoops: loops
	loops 
		ifNotEmpty: [ self decodeLoops: loops ]
		ifEmpty: [ self pc > endPC ifFalse: [ instructionStream interpretNextInstructionFor: self ] ].
	self interpret: endPC 