opening
buildContextMenu: announcement

	| menu isEmpty | 
	menu := MenuMorph new.
"	menu addTitle: 'Operations'."
		
	isEmpty := announcement editorMorph selectionIsEmpty.
	
	(menu add: 'Copy' target: announcement editorMorph selector: #copyToClipboard) enabled: isEmpty not.
	(menu add: 'Cut' target: announcement editorMorph selector: #cutToClipboard) enabled: isEmpty not.
	(menu add: 'Paste' target: announcement editorMorph selector: #pasteClipboard) enabled: Clipboard clipboardText isEmpty not.

	menu addLine.
	menu add: 'Do it' target: self selector: #invokeDoitIn: argument: announcement editorMorph.
	menu add: 'Print it' target: self selector: #invokePrintItIn: argument: announcement editorMorph.
	menu add: 'Inspect it' target: self selector: #invokeInspectItIn: argument: announcement editorMorph.
	
	menu popUpEvent: announcement event in: announcement editorMorph world.
	announcement editorMorph takeKeyboardFocus.