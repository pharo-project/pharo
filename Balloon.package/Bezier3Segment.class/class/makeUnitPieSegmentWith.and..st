utilities
makeUnitPieSegmentWith: point1 and: point2
	"Create a clockwise unit pie segment from point1 to point2, that is a pie segment for a circle centered at zero with radius one."
	| pt1 pt2 dir1 dir2 mid length scale cp1 cp2 pt3 magic |
	"point1 and point2 are the points on the unit circle
	for accuracy (or broken input), renormalize them."
	pt1 := point1 normalized.
	pt2 := point2 normalized.
	"compute the normal vectors - those are tangent directions for the bezier"
	dir1 := pt1 y negated @ pt1 x.
	dir2 := pt2 y negated @ pt2 x.
	"Okay, now that we have the points and tangents on the unit circle, let's do the magic. For fitting a cubic bezier onto a circle section we know that we want the end points be on the circle and the tangents to point towards the right direction (both of which we have in the above). What we do NOT know is how to scale the tangents so that midpoint of the bezier is exactly on the circle.
	The good news is that there is a linear relation between the length of the tangent vectors and the distance of the midpoint from the circle's origin. The bad news is that I don't know how to derive it analytically. So what I do here is simply sampling the bezier twice (not really - the first sample is free) and then to compute the distance from the sample."

	"The first sample is just between the two points on the curve"
	mid := pt1 + pt2 * 0.5.

	"The second sample will be taken from the curve with coincident control points at the intersection of dir1 and dir2, which simplifies significantly with a little understanding about trigonometry, since the angle formed between mid, pt1 and the intersection is the same as between the center, pt1 and mid."
	length := mid r.
	"length is not only the distance from the center of the unit circle but also the sine of the angle between the circle's center, pt1 and mid (since center is at zero and pt1 has unit length). Therefore, to scale dir1 to the intersection with dir2 we can use mid's distance from pt1 and simply divide it by the sine value."
	scale := (mid dist: pt1).
	length > 0.0 ifTrue:[ scale := scale / length].
	"now sample the cubic bezier (optimized version for coincident control points)"
	cp1 := pt1 + (dir1 * (scale * 0.75)).
	cp2 := pt2 - (dir2 * (scale * 0.75)).
	pt3 := cp1 + cp2 * 0.5.
	"compute the magic constant"
	scale := (pt3 - mid) r / scale.
	magic := 1.0 - length / scale.
	"and finally answer the pie segment"
	^self
		from: pt1
		via: pt1 + (dir1 * magic)
		and: pt2 - (dir2 * magic)
		to: pt2