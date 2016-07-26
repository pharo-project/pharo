builder
buildShortcut: aBuilder
	<keymap>
	(aBuilder shortcut: #firstChild) 
		category: #SmalltalkEditor
		default: self defaultKeyCombination
		do: [ :morph | morph standOutIntoFirstScope]
		description: 'Select the node scope reducing to the first child'


