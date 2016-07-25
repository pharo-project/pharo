accessing
midVertices
	"Return and array of midpoints for this line or closed curve"
	| midPts nextVertIx tweens |
	vertices size < 2
		ifTrue: [^ vertices].
	midPts := OrderedCollection new.
	nextVertIx := 2.
	tweens := OrderedCollection new.
	tweens add: vertices first asIntegerPoint.
	"guarantee at least two points."
	self
		lineSegmentsDo: [:p1 :p2 | 
			tweens addLast: p2 asIntegerPoint.
			p2 = (vertices atWrap: nextVertIx)
				ifTrue: ["Found endPoint."
					midPts addLast: (tweens atWrap: tweens size + 1 // 2)
							+ (tweens at: tweens size // 2 + 1) // 2.
					tweens := OrderedCollection new.
					tweens add: p2 asIntegerPoint.
					"guarantee at least two points."
					nextVertIx := nextVertIx + 1]].
	^ midPts asArray