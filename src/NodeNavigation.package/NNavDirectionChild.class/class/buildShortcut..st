builder
buildShortcut: aBuilder
	<keymap>
	(aBuilder shortcut: #selectChild)
		category: #SmalltalkEditor
		default: $o meta
		do: [ :morph | morph standOutIntoScope ]
		description: 'Select the node choosing the child'