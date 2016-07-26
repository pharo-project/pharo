accessing
modeListMenu
	"Answer the menu to be presented to select an editing mode"

	^ (PragmaMenuBuilder pragmaKeyword: #RubEditingModeMenu model: self) menu