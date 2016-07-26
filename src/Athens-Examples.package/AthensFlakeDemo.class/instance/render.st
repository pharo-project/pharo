as yet unclassified
render 

	surface drawDuring: [:canvas | 
		canvas pathTransform loadIdentity.
		surface clear. 
		
"		canvas 
			setPaint: Color white;
			drawShape: (0@0 corner: self extent ).
"
		canvas pathTransform
			 translateX: self extent x *0.5  Y:  self extent y * 0.5;
			scaleBy: 160. 
			
		canvas pathTransform  rotateByDegrees:  (frame * 0.5 + ((frame *0.1 + 1.7) cos * 5)).
		1 to: 3 do:[ :i |
			canvas pathTransform  rotateByDegrees: 120.
			self drawSpike: canvas.
		].
		 self drawCircles: canvas
	].
