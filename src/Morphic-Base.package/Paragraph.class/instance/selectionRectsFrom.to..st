selection
selectionRectsFrom: characterBlock1 to: characterBlock2 
	"Return an array of rectangles representing the area between the two character blocks given as arguments."
	| line1 line2 rects cb1 cb2 w |
	characterBlock1 <= characterBlock2
		ifTrue: [cb1 := characterBlock1.  cb2 := characterBlock2]
		ifFalse: [cb2 := characterBlock1.  cb1 := characterBlock2].
	cb1 = cb2 ifTrue:
		[w := self caretWidth.
		^ Array with: (cb1 topLeft - (w@0) corner: cb1 bottomLeft + ((w+1)@0))].
	line1 := self lineIndexForCharacter: cb1 stringIndex.
	line2 := self lineIndexForCharacter: cb2 stringIndex.
	line1 = line2 ifTrue:
		[^ Array with: (cb1 topLeft corner: cb2 bottomRight)].
	rects := OrderedCollection new.
	rects addLast: (cb1 topLeft corner: (lines at: line1) bottomRight).
	line1+1 to: line2-1 do: [ :i |
		| line |
		line := lines at: i.
		(line left = rects last left and: [ line right = rects last right ])
			ifTrue: [ "new line has same margins as old one -- merge them, so that the caller gets as few rectangles as possible"
					| lastRect |
					lastRect := rects removeLast.
					rects add: (lastRect bottom: line bottom) ]
			ifFalse: [ "differing margins; cannot merge"
					rects add: line rectangle ] ].

	rects addLast: ((lines at: line2) topLeft rectangle: cb2 bottomLeft).
	^ rects