logging
logStackToFileIfNeeded
	self class logDebuggerStackToFile ifFalse: [ ^self ].
	
	[[Smalltalk logError: name inContext: interruptedContext  ] ifError: [ ]]
		valueWithin: 100 milliSeconds onTimeout: [  ]