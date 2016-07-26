figures
figure9: aCanvas
	| pt ellipsePath patternPaint |
	ellipsePath := aCanvas createPath: [ :builder |
		builder
			moveTo: 10@200;
			cwArcTo: 360@0  angle: Float pi ;
			lineTo: -100@180;
			lineTo: -150@ -0;
			lineTo: -110@ -180].

	aCanvas setPaint: Color black.
	aCanvas drawShape: ellipsePath.

	aCanvas paintTransform loadIdentity.
	aCanvas paintTransform scaleBy: ((frame/200) sin abs)*3.
	
	
	patternPaint := aCanvas setPaint: esugBallon .
	patternPaint setExtend: #None.
	aCanvas draw.