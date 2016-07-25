matching
matchAgainst: aMatcher
	"Match if the predicate block evaluates to true when given the
	current stream character as the argument."

	| original result |
	original := aMatcher currentState.
	result := lookahead matchAgainst: aMatcher.
	aMatcher restoreState: original.
	^ result not 
		and: [ next matchAgainst: aMatcher ]