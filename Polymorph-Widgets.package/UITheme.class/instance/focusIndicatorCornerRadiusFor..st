defaults
focusIndicatorCornerRadiusFor: aMorph
	"Answer the default corner radius preferred for the focus indicator
	for the morph for themes that support this."

	^aMorph wantsRoundedCorners
		ifTrue: [6]
		ifFalse: [2]