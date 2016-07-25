privileged
predicatePartPredicate
	"Answer a predicate that tests all of my elements that cannot be
	enumerated."

	| predicates |
	predicates := elements reject: [:some | some isEnumerable].
	predicates isEmpty
		ifTrue: [^[:char | negated]].
	predicates size = 1
		ifTrue: [^negated
			ifTrue: [predicates first predicateNegation]
			ifFalse: [predicates first predicate]].
	predicates := predicates collect: [:each | each predicate].
	^negated
		ifFalse:
			[[:char | predicates contains: [:some | some value: char]]]
		ifTrue:
			[[:char | (predicates contains: [:some | some value: char]) not]]