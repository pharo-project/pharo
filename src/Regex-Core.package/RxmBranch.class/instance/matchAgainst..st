matching
matchAgainst: aMatcher
	"Match either `next' or `alternative'. Fail if the alternative is nil."

	^(next matchAgainst: aMatcher)
		or: [alternative notNil
			and: [alternative matchAgainst: aMatcher]]