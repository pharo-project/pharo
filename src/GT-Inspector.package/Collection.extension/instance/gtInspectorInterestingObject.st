*GT-Inspector
gtInspectorInterestingObject
	^ self size = 1
		ifTrue: [ self anyOne ]
		ifFalse: [ self ]