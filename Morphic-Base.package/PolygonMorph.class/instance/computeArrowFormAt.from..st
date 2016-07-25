private
computeArrowFormAt: endPoint from: priorPoint 
	"Compute a triangle oriented along the line from priorPoint to  
	endPoint. Then draw those lines in a form and return that  
	form, with appropriate offset"

	| p1 pts box arrowForm bb origin |
	pts := self arrowBoundsAt: endPoint from: priorPoint.
	box := ((pts first rectangle: pts last) encompass: (pts second)) expandBy: 1.
	arrowForm := Form extent: box extent asIntegerPoint.
	bb := (BitBlt toForm: arrowForm)
				sourceForm: nil;
				fillColor: Color black;
				combinationRule: Form over;
				width: 1;
				height: 1.
	origin := box topLeft.
	p1 := pts last - origin.
	pts do: 
			[:p | 
			bb drawFrom: p1 to: p - origin.
			p1 := p - origin].
	arrowForm convexShapeFill: Color black.
	^arrowForm offset: box topLeft