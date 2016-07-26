operations
writeSourceToLog
	logSource ifTrue: [
		myClass logMethodSource: text forMethod: compiledMethod
			inCategory: category withStamp: changeStamp
	].
