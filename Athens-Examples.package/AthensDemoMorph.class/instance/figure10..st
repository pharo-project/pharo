figures
figure10: aCanvas
	| pt ellipsePath patternPaint rectangle |
	ellipsePath := aCanvas createPath: [ :builder |
		builder
			moveTo: 0@200;
			cwArcTo: 240@0  angle: Float pi ;
			 cwArcTo: -240@0 angle: Float pi ].
	aCanvas setPaint: Color black.
	aCanvas drawShape: ellipsePath.
	rectangle := 250@40 extent: 140@300.
	aCanvas drawShape: rectangle.


	aCanvas paintTransform loadIdentity.
	aCanvas paintTransform scaleBy: ((frame/100) sin abs)*5.
	aCanvas paintTransform rotateByDegrees: (frame/100) sin * 360.
	
	patternPaint := aCanvas setPaint: pharoLogo.
	patternPaint setExtend: #Repeat.
	aCanvas drawShape: ellipsePath.
	
	aCanvas setPaint: patternPaint.
	patternPaint setExtend: #Reflect.
	aCanvas drawShape: rectangle.
