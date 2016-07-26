initialize
createNArgs: numArgs
	^ (1 to: numArgs)
		collect: [ :i | 
			self incrArgCount.
			builder codeArgument: 'arg' , argCount printString ]