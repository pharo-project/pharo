label-styles
dropListButtonLabelFor: aDropList
	"Answer the label for the button of the given drop list."

	^AlphaImageMorph new
		image: (ScrollBar
			arrowOfDirection: #bottom
			size: aDropList buttonWidth - 3
			color: aDropList paneColor darker);
		enabled: aDropList enabled