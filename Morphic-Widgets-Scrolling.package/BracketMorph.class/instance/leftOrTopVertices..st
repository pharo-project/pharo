geometry
leftOrTopVertices: r
	"Answer the vertices for a left or top bracket in the given rectangle."
	
	^self orientation == #vertical
		ifTrue: [{r topLeft - (0@1). r left + (r height // 2 + (r height \\ 2))@(r center y - (r height + 1 \\ 2)).
				r left + (r height // 2 + (r height \\ 2))@(r center y). r bottomLeft}]
		ifFalse: [{r topLeft. (r center x - (r width + 1 \\ 2))@(r top + (r width // 2 + (r width \\ 2))).
				r center x@(r top + (r width // 2 + (r width \\ 2))). r topRight}]