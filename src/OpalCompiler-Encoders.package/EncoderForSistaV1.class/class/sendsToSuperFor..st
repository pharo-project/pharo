compiled method support
sendsToSuperFor: compiledMethod
	"Answer whether the receiver sends any message to super."
	| scanner |
	scanner := InstructionStream on: compiledMethod.
	^ scanner scanFor: 
		[:instr |  instr = 235 ]