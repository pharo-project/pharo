morph creation
focusIndicatorMorphFor: aMorph
	"Answer a (cached) focus indicator for the given morph."

	(self focusIndicator isNil or: [
			self focusIndicator isMorph
				ifTrue: [self focusIndicator ~~ aMorph]
				ifFalse: [self focusIndicator key ~~ aMorph]])
		ifTrue: [self focusIndicator: aMorph ->(self newFocusIndicatorMorphFor: aMorph)].
	^self focusIndicator value 
		privateBounds: aMorph focusBounds