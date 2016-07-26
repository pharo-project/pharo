focus handling
defaultFocusMorph
	"Answer the morph that should have the keyboard
	focus by default when the dialog is opened."
	
	^ selectorField
		ifNil: [ super defaultFocusMorph ]