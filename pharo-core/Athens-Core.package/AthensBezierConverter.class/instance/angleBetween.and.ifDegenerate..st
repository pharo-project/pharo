helpers
angleBetween: p1 and: p2 ifDegenerate: aBlock

" Calculate an angle (in radians) between two vectors. 
Evaluate a block, in case if calculation not possible because one of the vectors has zero length "

	| x1 y1 x2 y2 dot2 n2 |
	x1 := p1 x.
	y1 := p1 y.
	x2 := p2 x.
	y2 := p2 y.
	
	dot2 := x1 * x2 + (y1 * y2).
	dot2 := dot2 * dot2.
	
	n2 := (x1*x1 + (y1*y1)) * (x2*x2 + (y2*y2)).
	
	n2 = 0 ifTrue: [ ^ aBlock value ].
	
	^ (dot2 / n2) arcCos