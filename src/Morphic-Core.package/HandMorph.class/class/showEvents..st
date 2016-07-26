utilities
showEvents: aBool
	"HandMorph showEvents: true"
	"HandMorph showEvents: false"
	ShowEvents := aBool.
	aBool ifFalse: [ World invalidRect: (0@0 extent: 500@120) ].