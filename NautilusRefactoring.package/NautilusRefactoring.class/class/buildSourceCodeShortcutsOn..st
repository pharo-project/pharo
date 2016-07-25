shortcuts
buildSourceCodeShortcutsOn: aBuilder
	<keymap>
	(aBuilder shortcut: #rename)
		category: #NautilusSourceCodeShortcuts
		default: $r meta
		do: [ :target | 
			target sourceTextModel hasUnacceptedEdits not
				ifTrue: [ target refactor renameTextSelection ] ]
		description: 'Rename the current selection'.
	(aBuilder shortcut: #extractToTemp)
		category: #NautilusSourceCodeShortcuts
		default: $x meta shift
		do: [ :target | 
			target sourceTextModel hasUnacceptedEdits not
				ifTrue: [ target refactor extractToTempTextSelection ] ]
		description: 'Extract to temp the current selection'.
	(aBuilder shortcut: #extractToMethod)
		category: #NautilusSourceCodeShortcuts
		default: $m meta shift
		do: [ :target | 
			target sourceTextModel hasUnacceptedEdits not
				ifTrue: [ target refactor extractToMethodTextSelection ] ]
		description: 'Extract to method the current selection'.
	(aBuilder shortcut: #format)
		category: #NautilusSourceCodeShortcuts
		default: PharoShortcuts current formatCodeShortcut
		do: [ :target | target refactor formatSourceCode ]