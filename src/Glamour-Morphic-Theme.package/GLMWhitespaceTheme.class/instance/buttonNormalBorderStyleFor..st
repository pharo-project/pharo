border-styles-buttons
buttonNormalBorderStyleFor: aButton
	"Return the normal button borderStyle for the given button."

	(aButton valueOfProperty: #noBorder ifAbsent: [false]) ifTrue: [
		^ SimpleBorder new width: 0; baseColor: Color transparent ].
	^ self glamorousNoBorderFor: aButton