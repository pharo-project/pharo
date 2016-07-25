border-styles-buttons
buttonNormalBorderStyleFor: aButton
	"Return the normal button borderStyle for the given button."

	| outerColor |
	(aButton valueOfProperty: #noBorder ifAbsent: [false]) ifTrue: [
		^ SimpleBorder new width: 0; baseColor: Color transparent ].
	outerColor := self glamorousDarkBaseColorFor: aButton.
	^SimpleBorder new
					width: 1;
					baseColor: outerColor