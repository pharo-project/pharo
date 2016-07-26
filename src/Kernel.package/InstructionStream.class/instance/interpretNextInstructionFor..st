decoding
interpretNextInstructionFor: client 
	"Send to the argument, client, a message that specifies the type of the 
	next instruction."
	^ self method encoderClass interpretNextInstructionFor: client  in: self