managing editing mode
modeListMenu
	"Answer the menu to be presented to select an editing mode"

	| menu |
	menu := MenuMorph new.
	menu add: 'Plain text' target: self selector: #switchToPlainTextMode.
	menu add: 'Smalltalk scripting' target: self selector: #switchToSmalltalkScriptingMode.
	^ menu