testing
includesClass: aClass 
	(super includesClass: aClass) ifFalse: [^false].
	(instanceVariables includesKey: aClass name) ifTrue: [^true].
	(classVariables includesKey: aClass name) ifTrue: [^true].
	^((self selectorCacheFor: aClass) 
		detect: [:each | self includesSelector: each in: aClass]
		ifNone: [nil]) notNil