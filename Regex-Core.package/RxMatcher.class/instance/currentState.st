privileged
currentState
	"Answer an opaque object that can later be used to restore the
	matcher's state (for backtracking)."

	| origPosition origLastChar |
	origPosition := stream position.
	^	[stream position: origPosition]