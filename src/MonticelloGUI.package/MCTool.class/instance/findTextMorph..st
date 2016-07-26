morphic ui
findTextMorph: aSymbol
	^ morph submorphs detect: [:ea | (ea respondsTo: #getTextSelector) and: [ea getTextSelector = aSymbol]] ifNone: []