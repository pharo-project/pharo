accessing
availableFillStyles
	"Return the selectors of the the available fill styles for each state.
	Must match the order of interactionStates."
	
	^#(normalFillStyle mouseOverFillStyle pressedFillStyle disabledFillStyle
		selectedFillStyle selectedPressedFillStyle selectedMouseOverFillStyle selectedDisabledFillStyle)