shortcut examples
examplesOfSmalltalkEditorShortcutsOn: aBuilder
	"No Keymap here, by default, the editor rely on a builtin keymapping engine (see TextEditor and SmallEditor)
	Here are some example on how to declare them if you want to setup different shortcuts

	<keymap>
	(aBuilder shortcut: #browseIt)
		category: #RubSmalltalkEditor
		default: $b command mac | $b ctrl win | $b ctrl unix
		do: [ :morph | morph editor browseIt ].
					
	(aBuilder shortcut: #doIt) 
		category: #RubSmalltalkEditor
		default: $d command mac | $d ctrl win | $d ctrl unix
		do: [ :morph | morph doIt ].
			
	(aBuilder shortcut: #inspectIt) 
		category: #RubSmalltalkEditor
		default: $i command mac | $i ctrl win | $i ctrl unix
		do: [ :morph | morph inspectIt ].
		
	(aBuilder shortcut: #implementorsOfIt) 
		category: #RubSmalltalkEditor
		default: $m command mac | $m ctrl win | $m ctrl unix
		do: [ :morph | morph implementorsOfIt ].
		
	(aBuilder shortcut: #sendersOfIt)
		category: #RubSmalltalkEditor
		default: $n command mac | $n ctrl win | $n ctrl unix
		do: [ :morph | morph sendersOfIt ].
		
	(aBuilder shortcut: #printIt) 
		category: #RubSmalltalkEditor
		default: $p command mac | $p ctrl win | $p ctrl unix
		do: [ :morph | morph printIt ].
		"