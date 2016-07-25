instructions
send: selector toSuperOf: behavior

	^self add: (IRInstruction send: selector toSuperOf: behavior)