keymapping
buildShortcutsOn: aBuilder
	<keymap>

	(aBuilder shortcut: #close)
		category: #WindowShortcuts
		default: PharoShortcuts current closeWindowShortcut
		do: [ :target | target closeWindowAction ]
		description: 'Close this window'.