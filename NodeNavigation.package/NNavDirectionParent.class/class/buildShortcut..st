builder
buildShortcut: aBuilder
	<keymap>

	(aBuilder shortcut: #parent)
		category: #SmalltalkEditor
		default: self defaultKeyCombination
		do: [ :morph | morph standOutOverScope ]
		description: 'Select the node scope going to the paren node'


