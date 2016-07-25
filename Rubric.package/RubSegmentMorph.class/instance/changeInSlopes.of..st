cubic support
changeInSlopes: slopes of: verts 
	"A message to knots of a spline. Returns an array with the 3rd cubic coeff."
	"The last nth item is correct iff this is a closed cubic.
	Presumably that is the only time we care.
	We always return the same sized array as self."
	| n slopeChanges |
	n := verts size.
	n = slopes size ifFalse: [^ self error: 'vertices and slopes differ in number'].
	slopeChanges := Array new: n.
	1 to: n do: [:i | slopeChanges at: i put: (verts atWrap: i + 1)
					- (verts at: i) * 3 - ((slopes at: i) * 2)
					- (slopes atWrap: i + 1)].
	^ slopeChanges