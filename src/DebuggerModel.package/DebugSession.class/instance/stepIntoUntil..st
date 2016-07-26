debugging actions
stepIntoUntil: aBlock
	[ aBlock cull: interruptedContext ]
		whileFalse: [ self stepInto: interruptedContext ]