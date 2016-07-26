submorphs-accessing
morphsInFrontOverlapping: aRectangle do: aBlock
	"Evaluate aBlock with all top-level morphs in front of someMorph that overlap with the given rectangle."
	^self morphsInFrontOf: nil overlapping: aRectangle do: aBlock