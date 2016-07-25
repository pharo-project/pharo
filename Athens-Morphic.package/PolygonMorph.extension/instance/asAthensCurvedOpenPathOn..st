*Athens-Morphic
asAthensCurvedOpenPathOn: anAthensCanvas
"like asAthensCurvedPathOn: but use quadratic curve for start and end points"
	| points |
	points := self computeCurvedSegmentCtrlPoints.
	^ anAthensCanvas
		createPath: [ :builder | 
			self vertices notEmpty
				ifTrue: [ 
					| prior |
					"points contains {ctrl1 vertice ctrl2}"
					prior := points first.
					builder absolute.
					builder moveTo: prior second.
					builder curveVia: (points at: 2) first to: (points at: 2) second.
					prior := points at: 2.
					3 to: points size - 1 do: [ :index | 
							| p |
							p := points at: index.
							builder curveVia: prior third and: p first to: p second.
							prior := p ].
					builder curveVia: prior third to: points last second].
			builder ]