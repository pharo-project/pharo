geometry
flipHAroundX: centerX
	"Flip me horizontally around the center.  If centerX is nil, compute my center of gravity."

	| cent |
	cent := centerX 
		ifNil: [ bounds center x ] "average is the center"
		ifNotNil: [ centerX ].
	self setVertices: (vertices collect: [ :vv | ((vv x - cent) * -1 + cent) @ vv y ]) reversed.