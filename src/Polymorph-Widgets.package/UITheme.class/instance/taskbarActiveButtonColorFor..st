basic-colors
taskbarActiveButtonColorFor: aButton
	"Answer the colour for the given active taskbar button."

	^self scrollbarColor alphaMixed: 0.7 with: Color white