builder
buildShortcut: aBuilder
	<keymap>

	(aBuilder shortcut: #previousSibling) 
		category: #SmalltalkEditor 
		default: self defaultKeyCombination 
		do: [ :morph | morph standOutPreviousChild]
		description: 'Select the node scope reducing to the previous sibling'


