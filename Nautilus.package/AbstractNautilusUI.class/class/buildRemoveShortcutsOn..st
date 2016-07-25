shortcuts
buildRemoveShortcutsOn: aBuilder
	<keymap>

	(aBuilder shortcut: #removeClass)
		category: #NautilusGlobalShortcuts
		default: $x meta  ,$c meta 
		do: [ :target | target removeClassWithRefactoringEngine: target selectedClasses asOrderedCollection ]
		description: 'Remove the selected classes'.
			
	(aBuilder shortcut: #removePackage)
		category: #NautilusGlobalShortcuts
		default: $x meta  , $p meta 
		do: [ :target | target removePackages ]
		description: 'Remove the selected packages'.

	(aBuilder shortcut: #removeProtocol)
		category: #NautilusGlobalShortcuts
		default: $x meta , $t meta 
		do: [ :target | target removeCategories ]
		description: 'Remove the selected protocols'.
		
	(aBuilder shortcut: #removeMethod)
		category: #NautilusGlobalShortcuts
		default: $x meta , $m meta 
		do: [ :target | target removeMethods ]
		description: 'Remove the selected methods'.
		
	(aBuilder shortcut: #removeGroups)
		category: #NautilusGlobalShortcuts
		default: $x meta , $g meta 
		do: [ :target | target removeGroups ]
		description: 'Remove the selected groups'.