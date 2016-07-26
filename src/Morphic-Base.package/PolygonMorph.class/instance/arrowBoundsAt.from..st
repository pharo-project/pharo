private
arrowBoundsAt: endPoint from: priorPoint 
	"Answer a triangle oriented along the line from priorPoint to endPoint."
	| d v angle wingBase arrowSpec length width |
	v := endPoint - priorPoint.
	angle := v degrees.
	d := borderWidth max: 1.
	arrowSpec := self valueOfProperty: #arrowSpec ifAbsent: [PolygonMorph defaultArrowSpec].
	length := arrowSpec x abs.  width := arrowSpec y abs.
	wingBase := endPoint + (Point r: d * length degrees: angle + 180.0).
	arrowSpec x >= 0
		ifTrue: [^ {	endPoint.
					wingBase + (Point r: d * width degrees: angle + 125.0).
					wingBase + (Point r: d * width degrees: angle - 125.0) }]
		ifFalse: ["Negative length means concave base."
				^ {	endPoint.
					wingBase + (Point r: d * width degrees: angle + 125.0).
					wingBase.
					wingBase + (Point r: d * width degrees: angle - 125.0) }]