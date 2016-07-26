accessing
predicateIgnoringCase: aBoolean

	| predicate enumerable |
	enumerable := self enumerablePartPredicateIgnoringCase: aBoolean.
	^self hasPredicates
		ifFalse: [enumerable]
		ifTrue:
			[predicate := self predicatePartPredicate.
			negated
				ifTrue: [[:char | (enumerable value: char) and: [predicate value: char]]]
				ifFalse: [[:char | (enumerable value: char) or: [predicate value: char]]]]