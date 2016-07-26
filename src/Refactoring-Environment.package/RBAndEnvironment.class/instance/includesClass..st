testing
includesClass: aClass 
	| doesntHaveSelectors |
	(environment includesClass: aClass) ifFalse: [^false].
	(andedEnvironment includesClass: aClass) ifFalse: [^false].
	doesntHaveSelectors := true.
	environment selectorsForClass: aClass
		do: 
			[:each | 
			doesntHaveSelectors := false.
			(andedEnvironment includesSelector: each in: aClass) ifTrue: [^true]].
	^doesntHaveSelectors