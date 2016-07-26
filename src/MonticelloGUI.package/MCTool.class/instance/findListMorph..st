morphic ui
findListMorph: aSymbol
	^ morph submorphs detect: [:ea | (ea respondsTo: #getListSelector) and: [ea getListSelector = aSymbol]] ifNone: []