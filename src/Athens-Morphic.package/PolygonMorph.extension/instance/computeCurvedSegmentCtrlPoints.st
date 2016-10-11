*Athens-Morphic
computeCurvedSegmentCtrlPoints
	"compute control points for the curved polygon.
For every vertice C take the two edges L and R. Compute the line T between the midpoints of L and R.
Use T is the tangent on the C. T is divided by the C with the same ration as L/R.
The start and end point of T defines the controlpoint c1 and c2
for the current vertice C.
"

	| ctrls vert |
	vert := self adjustSegmentPointsForArrows.
	ctrls := (1 to: vert size)
		collect: [ :i | 
			| prior current next lenpc lencn ctrl1 ctrl2 tangent |
			prior := vert atWrap: i - 1.
			current := vert atWrap: i.
			next := vert atWrap: i + 1.
			tangent := (next - prior) / 2.
			lenpc := current distanceTo: prior.
			lencn := next dist: current.
			lenpc = 0 ifTrue:[
				ctrl1 := prior]
			ifFalse:[	ctrl1 := current - (tangent / (1 + (lencn / lenpc)))].
				lencn = 0 ifTrue:[
					ctrl2 := next]
				ifFalse:[
			ctrl2 := current + (tangent / (1 + (lenpc / lencn)))].
		"collect ctrl1 current and ctrl2"
			{(ctrl1 asFloatPoint).
			current.
			(ctrl2 asFloatPoint)} ].
	^ ctrls