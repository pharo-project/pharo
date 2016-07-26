rectangle functions
quickMerge: aRectangle 
	"Answer the receiver if it encloses the given rectangle or the merge of the two rectangles if it doesn't. THis method is an optimization to reduce extra rectangle creations."
	| useRcvr rOrigin rCorner minX maxX minY maxY |
	
	aRectangle ifNil: [ ^self ].
	
	useRcvr := true.
	rOrigin := aRectangle topLeft.
	rCorner := aRectangle bottomRight.
	minX := rOrigin x < origin x 
		ifTrue: 
			[ useRcvr := false.
			rOrigin x ]
		ifFalse: [ origin x ].
	maxX := rCorner x > corner x 
		ifTrue: 
			[ useRcvr := false.
			rCorner x ]
		ifFalse: [ corner x ].
	minY := rOrigin y < origin y 
		ifTrue: 
			[ useRcvr := false.
			rOrigin y ]
		ifFalse: [ origin y ].
	maxY := rCorner y > corner y 
		ifTrue: 
			[ useRcvr := false.
			rCorner y ]
		ifFalse: [ corner y ].
	useRcvr 
		ifTrue: [ ^ self ]
		ifFalse: 
			[ ^ Rectangle 
				origin: minX @ minY
				corner: maxX @ maxY ]