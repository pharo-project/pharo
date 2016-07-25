private
arcTo: newEndPoint angle: angle cw: aBool

	" Add a clockwise arc segment, starting from current path endpoint and
	ending at andPt. Angle should be specified in radians 
	"

	| start end center v radius startAngle endAngle cwAngle |	
	lastControlPoint := nil.
	angle isZero ifTrue: [ ^ self lineTo: newEndPoint ].
	 
	start := endPoint.
	endPoint := end := self toAbsolute: newEndPoint.
	
	start = end ifTrue: [ ^ self ].

	"we have to transform the input. because Cario expects 
	the center , radius, starting and ending angle,
	and we have the starting point, the ending point , and the angle.
	"
	aBool ifTrue: [cwAngle := angle] ifFalse: [cwAngle := angle negated].
	center := self calcCenter: start end: end  angle: cwAngle.
	v := (start - center).
	radius := v r.
	startAngle := self angleOfVector: v.
	endAngle := self angleOfVector: (end-center).
	aBool ifTrue: [ self arcCenterX: center x centerY: center y  radius: radius  startAngle: startAngle endAngle: endAngle ]
		ifFalse: [ self arcNegativeCenterX: center x centerY: center y  radius: radius  startAngle: startAngle  endAngle: endAngle ]
		