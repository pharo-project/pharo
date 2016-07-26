testing
isFlatBezier2_x1: x1 y1: y1 x2: x2 y2: y2 x3: x3 y3: y3

	| dx dy d da angle |
	
	dx := x3-x1.
	dy := y3-y1.
	
 	d := (((x2 - x3) * dy) - ((y2 - y3) * dx)) abs.

	d > CollinearityEps ifTrue: [
		
		"regular case"

		d*d <= (distanceTolerance * ( dx*dx + (dy*dy))) ifTrue: [
			
			angleTolerance < CurveAngleTolerance ifTrue: [ ^ true ].
			
			angle := self angleBetween: x2-x1 @ (y2-y1) and: x3-x2 @ (y3-y2)
				ifDegenerate: [ 0.0 ].
			
			"parallel. no need to proceed further"
			angle <= angleTolerance ifTrue: [ ^ true ]					
		]
	]
	ifFalse: [ 
		"collinear"
		da := dx*dx + (dy*dy).
		
		da = 0 
			ifTrue: [ d := (x1-x2) squared + (y1-y2) squared ]
			ifFalse: [
				d = ((x2 - x1)*dx + ((y2 - y1)*dy)) / da.

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

			d < self distanceToleranceSquared ifTrue: [ ^ true ]		
	].

	^ false