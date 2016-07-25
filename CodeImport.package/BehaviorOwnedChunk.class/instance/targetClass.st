accessing
targetClass
	| target |
	target := self class environment at: behaviorName.
	^isMeta
		ifTrue: [ target classSide ]
		ifFalse: [ target ]