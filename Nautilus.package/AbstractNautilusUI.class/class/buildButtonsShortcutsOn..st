shortcuts
buildButtonsShortcutsOn: aBuilder
	<keymap>

	(aBuilder shortcut: #showInstanceSide)
		category: #NautilusGlobalShortcuts
		default: $t meta  ,$i meta 
		do: [ :target | target showInstance: true ]
		description: 'Show instance side'.

	(aBuilder shortcut: #showClassSide)
		category: #NautilusGlobalShortcuts
		default: $t meta  ,$c meta 
		do: [ :target | target showInstance: false ]
		description: 'Show class side'.

	(aBuilder shortcut: #showHierarchy)
		category: #NautilusGlobalShortcuts
		default: $t meta  ,$h meta 
		do: [ :target | target showHierarchy: true ]
		description: 'Show Hierarchy'.
		
	(aBuilder shortcut: #showFlat)
		category: #NautilusGlobalShortcuts
		default: $t meta  ,$f meta 
		do: [ :target | target showHierarchy: false ]
		description: 'Show normal view'.
