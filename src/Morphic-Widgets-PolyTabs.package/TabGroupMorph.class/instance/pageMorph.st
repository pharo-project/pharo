accessing
pageMorph
	"Answer the current page morph if any."

	^self contentMorph hasSubmorphs
		ifTrue: [self contentMorph submorphs first]