shortcuts
buildTextEditorShortcutsOn: aBuilder
	<keymap>
	
	(aBuilder shortcut: #toggleCommentOnSelectionOrLine)
		category: #TextEditor
		default: PharoShortcuts current toggleCommentOnSelectionOrLine
		do: [ :target  | target handleEdit: [ target toggleCommentOnSelectionOrLine ] ].
		
	(aBuilder shortcut: #selectAll) 
		category: #TextEditor
		default: PharoShortcuts current selectAllShortcut
		do: [ :target | target handleEdit: [ target selectAll ] ].

	(aBuilder shortcut: #copySelection) 
		category: #TextEditor
		default: PharoShortcuts current copySelectionShortcut
		do: [ :target | target copySelection ].
		
	(aBuilder shortcut: #find) 
		category: #TextEditor
		default: PharoShortcuts current findShortcut
		do: [ :target | target find ].
		
	(aBuilder shortcut: #findAgain) 
		category: #TextEditor
		default: PharoShortcuts current findAgainShortcut
		do: [ :target | target findAgain ].
		
	(aBuilder shortcut: #setSearchString)
		category: #TextEditor
		default: PharoShortcuts current setSearchStringShortcut
		do: [ :target | target setSearchString ].
		
	(aBuilder shortcut: #cancel) 
		category: #TextEditor
		default: PharoShortcuts current cancelShortcut
		do: [ :target | target cancelWithoutConfirmation ].
		
	(aBuilder shortcut: #paste) 
		category: #TextEditor
		default: PharoShortcuts current pasteShortcut
		do: [ :target | target paste ].
		
	(aBuilder shortcut: #cut) 
		category: #TextEditor
		default: PharoShortcuts current cutShortcut
		do: [ :target | target cut ].
				
	(aBuilder shortcut: #undo) 
		category: #TextEditor
		default: PharoShortcuts current undoShortcut
		do: [ :target | target undo ].
		
	(aBuilder shortcut: #redo) 
		category: #TextEditor
		default: PharoShortcuts current redoShortcut
		do: [ :target | target redo ].
	
	(aBuilder shortcut: #cursorHome)
		category: #TextEditor
		default: PharoShortcuts current cursorHomeShortcut
		do: [ :target :morph :event | target cursorHome: event ].
	
	(aBuilder shortcut: #cursorEnd)
		category: #TextEditor
		default: PharoShortcuts current cursorEndShortcut
		do: [ :target :morph :event | target cursorEnd: event ].