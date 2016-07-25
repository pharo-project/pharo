adding
add: anInstruction

	sequence add: anInstruction.
	anInstruction sequence: self.
	^anInstruction.