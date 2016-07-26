accessing
lastClickedMorph: aNodeMorph

	lastClickedMorph ifNotNil: [ lastClickedMorph complexContents lastClicked: false ].
	lastClickedMorph := aNodeMorph.
	aNodeMorph ifNotNil: [ aNodeMorph complexContents lastClicked: true ].