compiled method support
sendsToSuperFor: compiledMethod
	"Answer whether the receiver sends any message to super."
	| scanner |
	scanner := InstructionStream on: compiledMethod.
	^ scanner scanFor: 
		[:instr |  instr = 16r85 or: [instr = 16r84
						and: [scanner followingByte between: 16r20 and: 16r3F]]]