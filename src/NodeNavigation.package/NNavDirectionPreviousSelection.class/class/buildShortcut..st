builder
buildShortcut: aBuilder
	<keymap>

	(aBuilder shortcut: #previousSelection)
		category: #SmalltalkEditor
		default: $b meta shift 
		do: [ :morph | morph standOutPreviousSelection ]
		description: 'Select the previous selected scope'