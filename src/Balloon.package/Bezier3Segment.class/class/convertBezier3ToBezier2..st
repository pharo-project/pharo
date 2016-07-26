utilities
convertBezier3ToBezier2: vertices
	| pa pts index c |
	pts := OrderedCollection new.
	1 to: vertices size // 4 do:
		[:i |
		index := i * 4 - 3.
		c := Bezier3Segment new
					from: (vertices at: index)
					via: (vertices at: index + 1)
					and: (vertices at: index + 2)
					to: (vertices at: index + 3).
		pts addAll: c asBezierShape].
	pa := PointArray new: pts size.
	pts withIndexDo: [:p :i | pa at: i put: p ].
	^ pa