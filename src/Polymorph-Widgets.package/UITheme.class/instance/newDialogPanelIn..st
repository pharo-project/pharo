morph creation
newDialogPanelIn: aThemedMorph
	"Answer a new (main) dialog panel."

	^(self newPanelIn: aThemedMorph)
		fillStyle: (SolidFillStyle color: Color transparent) "no pane colour tracking"