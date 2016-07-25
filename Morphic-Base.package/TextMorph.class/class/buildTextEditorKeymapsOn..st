shortcuts
buildTextEditorKeymapsOn: aBuilder
	<keymap>
	
	(aBuilder shortcut: #accept)
		category: #TextMorph
		default: PharoShortcuts current acceptShortcut
		do: [ :morph | morph acceptContents ].