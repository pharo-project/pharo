events
handleEvent: anEvent

	(filterBlock isNil or: [ filterBlock value: anEvent ])
		ifTrue: [ anEvent asString logCr ]