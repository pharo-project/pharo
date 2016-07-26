morphic
findListContaining: aString
	^ self listMorphs detect: [:m | m getList includes: aString]