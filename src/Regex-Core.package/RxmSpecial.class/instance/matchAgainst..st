matching
matchAgainst: aMatcher
	"Match without consuming any input, if the matcher is
	in appropriate state."

	^(aMatcher perform: matchSelector)
		and: [next matchAgainst: aMatcher]