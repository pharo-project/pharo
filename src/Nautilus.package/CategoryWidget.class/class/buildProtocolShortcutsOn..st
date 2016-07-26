shortcuts
buildProtocolShortcutsOn: aBuilder
	<keymap>

	(aBuilder shortcut: #fullBrowse)
		category: #NautilusProtocolShortcuts
		default: $b meta
		do: [ :target | target fullBrowse ]
		description: 'Open a new browser on the selection'.
		
	(aBuilder shortcut: #restrictedBrowseClass)
		category: #NautilusProtocolShortcuts
		default: $b meta shift
		do: [ :target | target restrictedBrowseClass ]
		description: 'Open a restricted browser'.
		
	(aBuilder shortcut: #findMethod)
		category: #NautilusProtocolShortcuts
		default: $f meta
		do: [ :target | target findMethod ]
		description: 'Find a method'.
	
	(aBuilder shortcut: #addCategory)
		category: #NautilusProtocolShortcuts
		default: $n meta
		do: [ :target | target addCategory ]
		description: 'Add a new protocol'.
	
	(aBuilder shortcut: #renameCategory)
		category: #NautilusProtocolShortcuts
		default: $r meta
		do: [ :target | target enableCategorySingleSelection ifTrue: [target renameCategory ]]
		description: 'Rename the selected protocol'.
	
	(aBuilder shortcut: #removeCategories)
		category: #NautilusProtocolShortcuts
		default: $x meta
		do: [ :target | target removeCategories ]
		description: 'Remove the selected protocols'.