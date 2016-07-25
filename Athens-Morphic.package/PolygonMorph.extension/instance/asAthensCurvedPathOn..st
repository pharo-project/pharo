*Athens-Morphic
asAthensCurvedPathOn: anAthensCanvas
	| points |
	points := self computeCurvedSegmentCtrlPoints.
	^ anAthensCanvas
		createPath: [ :builder | 
			self vertices notEmpty
				ifTrue: [ 
					| prior | "points contains {ctrl1 vertice ctrl2}"
					prior := points first.
					builder absolute.
					builder moveTo: prior second.
					points allButFirst
						do: [ :p | 
							builder curveVia: prior third and: p first to: p second.
							prior := p ].
					self isClosed
						ifTrue: [ 
							builder curveVia: prior third and: points first first to: points first second.
							builder close ] ].
			builder ]