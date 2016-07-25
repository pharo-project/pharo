geometry
rightOrBottomVertices: r
	"Answer the vertices for a right or bottom bracket in the given rectangle."
	
	^self orientation == #vertical
		ifTrue: [{r topRight - (0@1). r right - (r height // 2 + (r height \\ 2))@(r center y - (r height + 1 \\ 2)).
				r right - (r height // 2 + (r height \\ 2))@(r center y). r bottomRight}]
		ifFalse: [{(r center x)@(r bottom - 1 - (r width // 2 + (r width \\ 2))).
				r center x @(r bottom - 1 - (r width // 2 + (r width \\ 2))). r bottomRight. r bottomLeft - (1@0)}]