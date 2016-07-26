subdivision
recursiveSubDiv: flattener level: level

	| dx dy d2 d3 da1 da2 k f1 f2 |
	
	
	" Try to approximate the full cubic curve by a single straight line "
	level > flattener subdivisionLimit ifTrue: [  ^ self ].
	
	dx := x4-x1.
	dy := y4-y1.

	d2 := ((x2 - x4) * dy - ((y2 - y4) * dx)) abs.
	d3 := ((x3 - x4) * dy - ((y3 - y4) * dx)) abs.

	f1 := d2 > flattener curveCollinearityEpsilon.
	f2 := d3 > flattener curveCollinearityEpsilon.
	

	f1 ifTrue: [ 
		f2 ifTrue: [
			" Regular case "
			
			(d2 + d3) squared <= (flattener distanceToleranceSquared * (dx squared + dy squared)) ifTrue: [ 
				"If the curvature doesn't exceed the distance_tolerance value
				we tend to finish subdivisions."
			"	flattener accountForAngleTolerance ifFalse: [
					^ flattener lineToX: (x2 interpolateTo: x3 at: 0.5) y: (y2 interpolateTo: y3 at: 0.5)
					].
			"
				"Angle & Cusp Condition"

				k := (y3-y2) arcTan: (x3 - x2).
				da1 := (k - ((y2-y1) arcTan: (x2-x1))) abs.
				da2 := ((y4-y3 arcTan: x4-x3) - k) abs.

				da1 >= Float pi ifTrue: [  da1 := Float pi*2 - da1 ].
				da2 >= Float pi ifTrue: [  da2 := Float pi*2 - da2 ].
			
				(da1 + da2) < flattener angleTolerance ifTrue: [ 
					" Finally we can stop the recursion "
					^ flattener lineToX: (x2 interpolateTo: x3 at: 0.5) y: (y2 interpolateTo: y3 at: 0.5)
					 ].
		 
				(flattener overCuspLimit: da1) ifTrue: [ ^ flattener lineToX: x2 y: y2 ].
            	   	(flattener overCuspLimit: da2) ifTrue: [ ^ flattener lineToX: x3 y: y3 ].
			]
			
		] ifFalse: [ 
			
			"p1,p3,p4 are collinear, p2 is significant"

			(d2 squared <= (flattener distanceToleranceSquared * (dx squared + dy squared) )  )
				ifTrue: [
					flattener accountForAngleTolerance ifFalse: [ 
						^ flattener lineToX: (x2 interpolateTo: x3 at: 0.5)  y: (y2 interpolateTo: y3 at: 0.5) ].

					"Angle Condition"
					da1 := ((y3-y2 arcTan: (x3-x2)) - (y2-y1 arcTan:(x2-x1))) abs.
					da1 >= Float pi ifTrue: [  da1 := Float pi * 2 - da1 ].
					
					da1 < flattener angleTolerance ifTrue: [ 
						^ flattener 
							lineToX: x2 y: y2;
							lineToX: x3 y: y3 ].
						
					(flattener overCuspLimit: da1) ifTrue: [ ^ flattener lineToX: x2 y: y2 ]
				]			
			 ]
	] ifFalse: [
		f2 ifTrue: [
		"p1,p2,p4 are collinear, p3 is significant "
			
			(d3 squared <= (flattener distanceToleranceSquared * (dx squared + dy squared) )  )
				ifTrue: [
					flattener accountForAngleTolerance ifFalse: [ 
						^ flattener lineToX: (x2 interpolateTo: x3 at: 0.5)  y: (y2 interpolateTo: y3 at: 0.5) ].

					"Angle Condition"
					da1 := ((y4-y3 arcTan: (x4-x3)) - (y3-y2 arcTan:(x3-x2))) abs.
					da1 >= Float pi ifTrue: [  da1 := Float pi * 2 - da1 ].
					
					da1 < flattener angleTolerance ifTrue: [ 
						^ flattener 
							lineToX: x2 y: y2;
							lineToX: x3 y: y3 ].
						
					(flattener overCuspLimit: da1) ifTrue: [ ^ flattener lineToX: x3 y: y3 ]
				]			
			  ] ifFalse: [
		"All collinear OR p1==p4 "
			k := dx*dx + (dy*dy).
			
			(k = 0.0) ifTrue: [
				d2 := (x1-x2) squared + (y1-y2) squared.
				d3 := (x3-x4) squared + (y3-y4) squared ] 
			ifFalse: [ 

				k := 1 / k.
				da1 := x2 - x1.
				da2 := y2 - y1.
				d2  := k * (da1*dx + (da2*dy)).
				da1 := x3 - x1.
				da2 := y3 - y1.
				d3  := k * (da1*dx + (da2*dy)).
				
				(d2 > 0.0 and: [ d2 < 1.0 and: [d3>0.0 and: [d3 < 1.0]]]) ifTrue: [ 
						" Simple collinear case, 1---2---3---4  We can leave just two endpoints"
							^ self ].
				d2 <= 0.0 
					ifTrue: [ d2 := (x1-x2) squared + (y1-y2) squared ]
					ifFalse: [  
						d2 >= 1.0 
							ifTrue: [ d2 := (x2-x4) squared + (y2-y4) squared ]
							ifFalse: [ d2 := (x2 - x1 - (d2*dx)) squared + (y2 - y1 - (d2*dy)) squared ]].

				d3 <= 0.0 
					ifTrue: [  d3 := (x3-x1) squared + (y3-y1) squared ]
					ifFalse: [ 
						d3 >= 1.0 
							ifTrue: [ d3 := (x3-x4) squared + (y3-y4) squared   ]
							ifFalse: [ d3 := (x3 - x1 - (d3*dx)) squared + (y3-y1- (d3*dy)) squared ]].
			].

			(d2 > d3) ifTrue: [
				(d2 < flattener distanceToleranceSquared)
					ifTrue: [ ^ flattener lineToX: x2 y: y2 ]
				]
			ifFalse: [ 
				(d3 < flattener distanceToleranceSquared )
					ifTrue: [  ^ flattener lineToX: x3 y: y3 ]
				 ].
		]
	].
	self subdivideAt: 0.5 do:  [  :b1 :b2 |
		b1 recursiveSubDiv: flattener level: level +1.	
		b2 recursiveSubDiv: flattener level: level +1.	
	].
	