protocol
toDosOf: aRule

	aRule ifNil: [ ^ {}].
	^ aRule isComposite
		ifTrue: [ aRule leaves flatCollect: [:rule | self toDosOf: rule]]
		ifFalse: [toDos at: aRule ifAbsent: [{}]]