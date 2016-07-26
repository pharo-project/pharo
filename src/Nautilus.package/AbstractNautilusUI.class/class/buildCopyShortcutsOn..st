shortcuts
buildCopyShortcutsOn: aBuilder
	<keymap>

	(aBuilder shortcut: #copyClass)
		category: #NautilusGlobalShortcuts
		default: $c meta , $c meta 
		do: [ :target | target copyClasses ]
		description: 'Copy the selected classes'.