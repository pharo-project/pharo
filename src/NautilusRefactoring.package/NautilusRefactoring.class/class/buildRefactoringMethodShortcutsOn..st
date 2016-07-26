shortcuts
buildRefactoringMethodShortcutsOn: aBuilder
	<keymap>	

	(aBuilder shortcut: #rename)
		category: #NautilusGlobalShortcuts
		default: $r meta,  $m meta
		do: [:target || scroll |
				scroll := target methodWidget vScrollValue.
				target refactor renameMethodFor: target selectedMethod.
				target methodWidget vScrollValue: scroll]
		description: 'Rename the seleted method'
		