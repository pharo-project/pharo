helpers
isFlatQuadBezier: x1 y1: y1 x2: x2 y2: y2 x3: x3 y3: y3

	| dx dy d da |
	
	dx := x3-x1.
	dy := y3-y1.
	
	"This is the area of triangle enclosing curve * 2"
 	d := (((x2 - x3) * dy) - ((y2 - y3) * dx)) abs.

	d > (self lengthToleranceSquared ) ifTrue: [
		
		"Non-collinear case (regular one)"
		| dot |

		"if dot product is close to zero, that means we having flat curve"			
		dot := ( (x2-x1)*(x2-x3) + ((y1-y2)*(y2-y3)) ) abs.
		dot < (self lengthToleranceSquared * 0.5 ) ifTrue: [  ^ true ].
	]
	ifFalse: [ 
		"collinear"
		da := dx*dx + (dy*dy).
		
		da = 0 "end points coincide"
			ifTrue: [ d := (x1-x2) squared + (y1-y2) squared  "pointy case" ]
			ifFalse: [
				"the control point lies on line between endpoints?"
				d := ((x2 - x1)*dx + ((y2 - y1)*dy)) / da.

				(d > 0.0 and: [ d < 1.0 ] ) ifTrue: [ 
					"Simple collinear case, 1---2---3" 
					^ true
     				].
				d <= 0.0 
					ifTrue: [ d := (x1-x2) squared + (y1-y2) squared ]
					ifFalse: [
						d >= 1.0 
							ifTrue: [ d:= (x2-x3) squared + (y2-y3) squared ]
							ifFalse: [ d:= (x2 - x1 - (d*dx)) squared + (y2 - y1 - (d*dy)) squared ]
					].
			].

			d < (self lengthToleranceSquared) ifTrue: [ ^ true ]		
	].

	^ false