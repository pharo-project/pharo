shortcuts
buildRefactoringShortcutsOn: aBuilder
	<keymap>	

	(aBuilder shortcut: #generateAccessors)
		category: #NautilusGlobalShortcuts
		default: $h meta,  $a meta
		do: [:target | target refactor generateAccessors ]
		description: 'Generate the accessors for the selected class'.
		
	(aBuilder shortcut: #generateSubclass)
		category: #NautilusGlobalShortcuts
		default: $h meta,  $n meta, $s meta
		do: [:target | target refactor generateSubclass ]
		description: 'Generate a subclass for the selected class'.