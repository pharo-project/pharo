settings
serverMode: aBoolean
	(aBoolean = true or: [aBoolean = false or: [aBoolean isNil]])
		ifTrue: [ServerMode := aBoolean]