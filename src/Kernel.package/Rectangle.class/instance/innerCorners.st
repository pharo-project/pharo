accessing
innerCorners
	"Return an array of inner corner points,
	ie, the most extreme pixels included,
	in the order of a quadrilateral spec for WarpBlt"
	| r1 |
	r1 := self topLeft corner: self bottomRight - (1 @ 1).
	^ Array 
		with: r1 topLeft
		with: r1 bottomLeft
		with: r1 bottomRight
		with: r1 topRight