menu declaration
shiftedYellowButtonMenu
	"Answer the menu to be presented when the yellow button is pressed while the shift key is down"

	^ (PragmaMenuBuilder pragmaKeyword: 'textEditorShiftedMenu' model: nil) menu