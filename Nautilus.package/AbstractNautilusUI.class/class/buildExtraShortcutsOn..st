shortcuts
buildExtraShortcutsOn: aBuilder
	<keymap>
	(aBuilder shortcut: #openBrowser)
		category: #NautilusGlobalShortcuts
		default: $o meta , $o meta
		do: [ :target | target fullBrowse ]
		description: 'Open a new browser'.
	(aBuilder shortcut: #openBrowserOnSubclass)
		category: #NautilusGlobalShortcuts
		default: $o meta , $s meta
		do: [ :target | target openSubclass ]
		description: 'Open a new browser on a subclass'.
	(aBuilder shortcut: #openBrowserOnSuperclass)
		category: #NautilusGlobalShortcuts
		default: $o meta , $s meta shift
		do: [ :target | target openSuperclass ]
		description: 'Open a new browser on a superclass'.
	(aBuilder shortcut: #toggleBytecodes)
		category: #NautilusGlobalShortcuts
		default: $b meta , $b meta
		do: [ :target | target toggleBytecodes ]
		description: 'ToggleBytecodes'