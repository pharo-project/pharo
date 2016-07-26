figures
figure5: aCanvas
	| wheels |
	self figure4: aCanvas.

	wheels := aCanvas createPath: [:builder |
		builder
			moveTo:  100@100;		
			cwArcTo: 60@0 angle: 90 degreesToRadians;
			moveTo:  80@0;				
			cwArcTo: 60@0 angle: 90 degreesToRadians;
			moveTo:  0.0@200;
			cwArcTo: -60@0 angle: 90 degreesToRadians;
			moveTo:  -80@0; 
			cwArcTo: -60@0 angle: 90 degreesToRadians ].

	aCanvas	
		setPaint: self wheelColor;
		drawShape: wheels