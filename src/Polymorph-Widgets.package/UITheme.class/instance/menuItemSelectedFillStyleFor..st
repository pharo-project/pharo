fill-styles-buttons
menuItemSelectedFillStyleFor: aMenuItem
	"Answer the selected fill style to use for the given menu item."
	
	Display depth <= 2 ifTrue: [^ Color gray].
	^ self settings menuSelectionColor.
