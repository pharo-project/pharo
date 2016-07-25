selection
defaultCharacterBlock
	^ (CharacterBlock new stringIndex: firstCharacterIndex text: text
			topLeft: lines first topLeft extent: 0 @ 0)
		textLine: lines first