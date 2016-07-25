builder
buildShortcut: aBuilder
	<keymap>

	(aBuilder shortcut: #sibling)
		category: #SmalltalkEditor
		default: self defaultKeyCombination 
		do: [ :morph | morph standOutHorizontalScope]
		description: 'Select the node going to the siblings'


