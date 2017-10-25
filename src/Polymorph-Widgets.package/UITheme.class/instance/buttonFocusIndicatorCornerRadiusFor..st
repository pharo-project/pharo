defaults
buttonFocusIndicatorCornerRadiusFor: aButton
	"Answer the default corner radius preferred for the focus indicator
	for the button for themes that support this."

	^aButton wantsRoundedCorners
		ifTrue: [7]
		ifFalse: [2]