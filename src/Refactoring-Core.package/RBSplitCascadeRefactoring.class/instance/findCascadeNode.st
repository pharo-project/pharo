preconditions
findCascadeNode
	"Find the cascade to be split."

	cascadeNode := self parseTree bestNodeFor: selectedInterval.
	[ cascadeNode isNil or: [ cascadeNode isCascade ] ]
		whileFalse: [ cascadeNode := cascadeNode parent ].
	cascadeNode isNil
		ifTrue: [ self refactoringFailure: 'The selection doesn''t appear to be within a cascade' ]