accessing
selectionRectsFrom: characterBlock1 to: characterBlock2
	"Return an array of rectangles representing the area between the two character blocks given as arguments."

	| line1 line2 rects cb1 cb2 w |
	characterBlock1 = characterBlock2
		ifTrue: [ ^ #() ].
	characterBlock1 <= characterBlock2
		ifTrue: [ 
			cb1 := characterBlock1.
			cb2 := characterBlock2 ]
		ifFalse: [ 
			cb2 := characterBlock1.
			cb1 := characterBlock2 ].
	line1 := self lineIndexOfCharacterIndex: cb1 stringIndex.
	line2 := self lineIndexOfCharacterIndex: cb2 stringIndex.
	line1 = line2
		ifTrue: [ ^ Array with: (cb1 topLeft corner: cb2 bottomRight) ].
	^ Array
		streamContents: [ :strm | 
			| last |
			strm nextPut: (last := cb1 topLeft corner: (self lines at: line1) bottomRight).
			line1 + 1 to: line2 - 1 do: [ :i | 
				| line |
				line := self lines at: i.
				(line left = last left and: [ line right = last right ])
					ifTrue: [ 
						"new line has same margins as old one -- "
						"merge them, so that the caller gets as few rectangles as possible"
						last privateSetCorner: last right @ line bottom ]
					ifFalse: [ 
						"differing margins; cannot merge"
						strm nextPut: (last := line rectangle) ] ].
			strm nextPut: ((self lines at: line2) topLeft corner: cb2 bottomLeft) ]