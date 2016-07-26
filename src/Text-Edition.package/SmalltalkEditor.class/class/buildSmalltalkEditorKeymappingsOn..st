shortcuts
buildSmalltalkEditorKeymappingsOn: aBuilder
	<keymap>
	(aBuilder shortcut: #browseIt)
		category: #SmalltalkEditor
		default: PharoShortcuts current browseShortcut
		do: [ :morph | morph browseIt ].
	(aBuilder shortcut: #doIt)
		category: #SmalltalkEditor
		default: PharoShortcuts current doItShortcut
		do: [ :morph | morph doIt ].
	(aBuilder shortcut: #inspectIt)
		category: #SmalltalkEditor
		default: PharoShortcuts current inspectItShortcut
		do: [ :morph | morph inspectIt ].
	(aBuilder shortcut: #implementorsOfIt)
		category: #SmalltalkEditor
		default: PharoShortcuts current browseImplementorsShortcut
		do: [ :morph | morph implementorsOfIt ].
	(aBuilder shortcut: #sendersOfIt)
		category: #SmalltalkEditor
		default: PharoShortcuts current browseSendersShortcut
		do: [ :morph | morph sendersOfIt ].
	(aBuilder shortcut: #formatCode)
		category: #SmalltalkEditor
		default: PharoShortcuts current formatCodeShortcut
		do: [ :morph | morph formatSourceCode ].
	(aBuilder shortcut: #printIt)
		category: #SmalltalkEditor
		default: PharoShortcuts current printItShortcut
		do: [ :morph | morph printIt ]