data flow instructions
extractBlockWith: copied numArgs: numArgs blockSize: blockSize
	| startPC numTemps |
	numTemps := instructionStream method encoderClass 
		numLocalTempsForBlockAt: self pc 
		in: instructionStream method.
	self initializeStackNumArgs: numArgs copied: copied numTemps: numTemps.
	startPC := self pc.
	self skipTemps: numTemps.
	self interpret: startPC + blockSize - 1.
	^ builder 
		codeBlock: currentSequence 
		arguments: (simulatedStack first: numArgs)