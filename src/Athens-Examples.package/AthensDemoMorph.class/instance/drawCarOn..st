drawing
drawCarOn: aCanvas
	"chasis -----------------------------------------------------------------------"
		| p stroke wheel decorator |
	p := aCanvas createPath:[:b|
			b 
				moveTo: 0.3@0.3 ;
				lineTo: 0.4@0;
				curveVia: 0.1@0 to: 0.1@0.1;
				lineTo: 0@0.2;
				curveVia: 0@0.1 to: -0.1@0.1;
				lineTo: -0.4@0;
				curveVia: -0.1@0 to: (0.1@0.1) negated;
				lineTo: 0@0.2 negated ;
				curveVia: 0@0.1 negated to: 0.1@0.1 negated ].
		aCanvas	
			setPaint: self coloredCarColor;
			drawShape: p.
		stroke := aCanvas setStrokePaint: self wheelColor.
		stroke width: 0.01.
		aCanvas draw.
		
		"wheels-----------------------------------------------------------------------"
		wheel := aCanvas createPath:[:b|
			b
				moveTo: 	0.3@0.3;		
				cwArcTo: 0.15@0 angle: Float halfPi ;
				moveTo: 	0.1@0;				
				cwArcTo: 0.15@0 angle: Float halfPi ;					
				moveTo: 	0.0@0.4;				
				cwArcTo: 0.15 negated@0 angle: Float halfPi; 
				moveTo: 	-0.1@0; 
				cwArcTo: 0.15 negated@0 angle: Float halfPi 	].
		aCanvas	
			setPaint: self wheelColor;
			drawShape: wheel.
		
		"chasis decorator-----------------------------------------------------------------------"
		decorator  := aCanvas createPath:[:b|
			b
				moveTo: 	0.2@0.45;	
				lineTo: 0.225@0 ;		
				cwArcTo: 0.15@0 angle: Float halfPi ;
				lineTo: 	0.225@0;				
				lineTo: 0@0.1 ;
				lineTo: -0.225@ 0;
				cwArcTo: -0.15@0 angle: Float halfPi ;
				lineTo: -0.225@ 0	;
				lineTo: 0@0.1 negated].
		stroke := aCanvas setStrokePaint: self chasisDecoratorColor.
		stroke width: 0.01.	
		aCanvas drawShape: decorator.
		aCanvas setPaint: self chasisLineColor.
		aCanvas draw.		
			
			