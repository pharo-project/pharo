shortcuts
buildRecategorizeShortcutsOn: aBuilder
	<keymap>

	(aBuilder shortcut: #recategorizeMethod)
		category: #NautilusGlobalShortcuts
		default: $m meta , $m meta 
		do: [ :target | target categorizeMethod ]
		description: 'Categorize the selected method'.
		
	(aBuilder shortcut: #recategorizeClass)
		category: #NautilusGlobalShortcuts
		default: $m meta , $c meta 
		do: [ :target | target moveInPackage ]
		description: 'Categorize the selected class'.