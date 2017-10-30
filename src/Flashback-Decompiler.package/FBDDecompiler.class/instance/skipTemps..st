data flow instructions
skipTemps: numTemps
	self flag: #FIXME. "should dispatch to the encoder"
	instructionStream method encoderClass name = #EncoderForSistaV1
		ifTrue: [ numTemps > 0 ifTrue: [ instructionStream nextInstruction ] ]
		ifFalse: [ numTemps timesRepeat: [ instructionStream nextInstruction ] ]
	