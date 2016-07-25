matching
matchAgainst: aMatcher
	"If a link does not match the contents of the matcher's stream,
	answer false. Otherwise, let the next matcher in the chain match."

	^next matchAgainst: aMatcher