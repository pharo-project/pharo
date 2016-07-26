menus
addCustomMenuItems: aMenu hand: aHand
	super addCustomMenuItems: aMenu hand: aHand.
	aMenu addUpdating: #opacityString selector: #changeOpacity