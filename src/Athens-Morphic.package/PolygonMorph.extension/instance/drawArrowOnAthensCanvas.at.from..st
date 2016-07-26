*Athens-Morphic
drawArrowOnAthensCanvas: anAthensCanvas at: endPoint from: priorPoint
	"Draw a triangle oriented along the line from priorPoint to  
	endPoint. Answer the wingBase."

	| pts spec wingBase path |
	pts := self arrowBoundsAt: endPoint from: priorPoint.
	wingBase := pts size = 4
		ifTrue: [ pts third ]
		ifFalse: [ (pts copyFrom: 2 to: 3) average ].
	spec := self valueOfProperty: #arrowSpec ifAbsent: [ PolygonMorph defaultArrowSpec ].
	path := anAthensCanvas
		createPath: [ :builder | 
			builder absolute.
			builder moveTo: pts first.
			pts allButFirst do: [ :p | builder lineTo: p ]. builder close. ].
	spec x sign = spec y sign
		ifTrue: [ 
			anAthensCanvas setPaint: self borderColor]
		ifFalse: [ 
			(anAthensCanvas
				setStrokePaint: self borderColor)
				width: (borderWidth + 1) // 2].
	anAthensCanvas drawShape: path.
	^ wingBase