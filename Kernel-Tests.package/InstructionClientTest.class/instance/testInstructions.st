tests
testInstructions
	"just interpret all of methods of Object"

	| client |
	client := InstructionClient new.
	Object methods
		do: [ :method | 
			| scanner |
			scanner := InstructionStream on: method.
			[ scanner pc <= method endPC ] whileTrue: [ scanner interpretNextInstructionFor: client ] ]