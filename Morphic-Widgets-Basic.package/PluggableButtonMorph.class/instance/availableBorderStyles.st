accessing
availableBorderStyles
	"Return the selectors of the the available border styles for each state.
	Must match the order of interactionStates."
	
	^#(normalBorderStyle mouseOverBorderStyle pressedBorderStyle disabledBorderStyle
		selectedBorderStyle selectedPressedBorderStyle selectedMouseOverBorderStyle selectedDisabledBorderStyle)