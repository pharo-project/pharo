stepping
localHaloBoundsFor: aMorph
	"aMorph may be in the hand and perhaps not in our world"

	| r |
	r := (self worldBoundsForMorph: aMorph) truncated.
	aMorph world = self world ifFalse: [^r].
	^((self transformFromOutermostWorld) globalBoundsToLocal: r) truncated