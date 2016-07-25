instance creation
newCharacterBlockIndex: anInteger text: aText
	| line |
	line := (RubTextLine start: 1 stop: 33 internalSpaces: 0 paddingWidth: 0) rectangle: (0 @ 0 corner: 0 @ 0).
	^ (CharacterBlock new stringIndex: anInteger text: aText topLeft: 0@0 extent: 0 @ 0)
			textLine: line;
			yourself