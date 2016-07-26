menu
editingModeMenuOn: aBuilder
	"Specify the menu used to select an editing mode"

	<contextMenu>
	<RubEditingModeMenu>
	(aBuilder item: self label translated)
		action: [:model | model textArea editingMode: self new ]