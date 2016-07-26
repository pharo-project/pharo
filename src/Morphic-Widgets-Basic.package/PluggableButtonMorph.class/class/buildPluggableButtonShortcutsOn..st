shortcuts
buildPluggableButtonShortcutsOn: aBuilder
	<keymap>
	(aBuilder shortcut: #action1)
		category: #PluggableButtonMorph
		default: Character space asKeyCombination | Character cr asKeyCombination
		do: [ :target :morph :event | morph performAction ].