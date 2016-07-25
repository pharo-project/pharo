drawing
computeVertices
	| lpos rpos fln lln |
	super computeVertices.
	fln := self firstLineIndex.
	lln := self lastLineIndex.
	underlineShape := OrderedCollection new.
	fln to: lln do: [ :idx | 
		| line cidx |
		line := self lines at: idx.
		cidx := idx = fln
			ifTrue: [ firstIndex ]
			ifFalse: [ line first ].
		lpos := (self characterBlockForIndex: cidx) bottomLeft.
		cidx := idx = lln
			ifTrue: [ lastIndex ]
			ifFalse: [ line last ].
		rpos := (self characterBlockForIndex: cidx) bottomLeft.
		underlineShape add: (self underlineShapeFromPosition: lpos toPosition: rpos) ]