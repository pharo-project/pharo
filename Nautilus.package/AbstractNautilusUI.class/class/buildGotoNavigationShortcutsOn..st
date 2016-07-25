shortcuts
buildGotoNavigationShortcutsOn: aBuilder
	<keymap>

	(aBuilder shortcut: #jumpToClass)
		category: #NautilusGlobalShortcuts
		default: $g meta , $c meta 
		do: [ :target | target giveFocusToClass ]
		description: 'Jump to the selected class'.
			
	(aBuilder shortcut: #jumpToPackage)
		category: #NautilusGlobalShortcuts
		default: $g meta, $p meta 
		do: [ :target | target giveFocusToPackage ]
		description: 'Jump to the selected package'.

	(aBuilder shortcut: #jumpToProtocol)
		category: #NautilusGlobalShortcuts
		default: $g meta , $t meta 
		do: [ :target | target giveFocusToProtocol ]
		description: 'Jump to the selected protocol'.
		
	(aBuilder shortcut: #jumpToMethod)
		category: #NautilusGlobalShortcuts
		default: $g meta , $m meta  
		do: [ :target | target giveFocusToMethod ]
		description: 'Jump to the selected method'.
		
	(aBuilder shortcut: #jumpToTestClass)
		category: #NautilusGlobalShortcuts
		default: $g meta , $j meta , $c meta
		do: [ :target | target createTestForSelectedClass ]
		description: 'Jump to test class'.