accessing
portIfAbsent: valuable
	^ self hasPort
		ifTrue: [ self port ]
		ifFalse: valuable