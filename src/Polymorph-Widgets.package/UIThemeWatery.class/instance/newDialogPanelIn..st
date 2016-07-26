morph creation
newDialogPanelIn: aThemedMorph
	"Answer a new (main) dialog panel."

	^(super 
		newDialogPanelIn: aThemedMorph)
		fillStyle: (SolidFillStyle color: Color transparent) "no pane colour tracking"