geometry
flipVAroundY: centerY
	"Flip me vertically around the center.  If centerY is nil, compute my center of gravity."

	| cent |
	cent := centerY 
		ifNil: [ bounds center y ]		"average is the center"
		ifNotNil: [ centerY ].
	self setVertices: (vertices collect: [:vv |
			vv x @ ((vv y - cent) * -1 + cent)]) reversed.