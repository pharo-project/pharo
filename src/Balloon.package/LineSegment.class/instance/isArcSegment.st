testing
isArcSegment
	"Answer whether I approximate an arc segment reasonably well"
	| mid v1 v2 d1 d2 center |
	start = end ifTrue:[^false].
	mid := self valueAt: 0.5.
	v1 := (start + mid) * 0.5.
	v2 := (mid + end) * 0.5.
	d1 := mid - start. d1 := d1 y @ d1 x negated.
	d2 := end - mid.  d2 := d2 y @ d2 x negated.

	center := LineSegment
		intersectFrom: v1 with: d1 to: v2 with: d2.

	"Now see if the tangents are 'reasonably close' to the circle"
	d1 := (start - center) normalized dotProduct: self tangentAtStart normalized.
	d1 abs > 0.02 ifTrue:[^false].
	d1 := (end - center) normalized dotProduct: self tangentAtEnd normalized.
	d1 abs > 0.02 ifTrue:[^false].
	d1 := (mid - center) normalized dotProduct: self tangentAtMid normalized.
	d1 abs > 0.02 ifTrue:[^false].

	^true