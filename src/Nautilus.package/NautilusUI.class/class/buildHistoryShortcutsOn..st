keymapping
buildHistoryShortcutsOn: aBuilder
	<keymap>

	(aBuilder shortcut: #previous)
		category: #NautilusGlobalShortcuts
		default: $[ meta shift
		do: [:target| target previous ]
		description: 'Go back in the navigation history.'.
		
	(aBuilder shortcut: #next)
		category: #NautilusGlobalShortcuts
		default: $] meta shift
		do: [:target| target next ]
		description: 'Go forward in the navigation history'.
	