morphic
findButtonWithLabel: aString
	^ self buttonMorphs detect: [:m | m label = aString]