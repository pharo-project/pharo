testing
includesClass: aClass
	(environment includesClass: aClass) ifTrue: [ ^ true ].
	(orEnvironment includesClass: aClass) ifTrue: [ ^ true ].
	(environment selectorsForClass: aClass) isEmpty ifFalse: [ ^ true ].
	(orEnvironment selectorsForClass: aClass) isEmpty ifFalse: [ ^ true ].
	^ false