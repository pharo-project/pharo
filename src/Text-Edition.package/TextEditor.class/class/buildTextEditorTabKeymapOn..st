shortcuts
buildTextEditorTabKeymapOn: aBuilder
	"This is to mask the tab or shift tab character at the system window level."
	<keymap>
	
	(aBuilder shortcut: #doTab) 
		category: #TextEditorTab
		default: Character tab asKeyCombination
		do: [ :target | target handleEdit: [ target
				handleEdit: [ 
					target replaceSelectionWith: '	'.
					target selectInvisiblyFrom: target selectionInterval last + 1 to: target selectionInterval last ] ] ]