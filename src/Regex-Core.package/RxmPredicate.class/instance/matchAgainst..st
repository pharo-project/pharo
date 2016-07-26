matching
matchAgainst: aMatcher
	"Match if the predicate block evaluates to true when given the
	current stream character as the argument."

	| original |
	original := aMatcher currentState.
	(aMatcher atEnd not 
		and: [(predicate value: aMatcher next)
			and: [next matchAgainst: aMatcher]])
		ifTrue: [^true]
		ifFalse:
			[aMatcher restoreState: original.
			^false]