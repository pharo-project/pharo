figures
figure3: aCanvas
	| pt path |
	
	path := aCanvas createPath: [:builder |
		builder
			relative;
			moveTo: 0.3@0.3 ;
			lineTo: 0.4@0;
			curveVia: 0.1@0 to: 0.1@0.1;
			lineTo: 0@0.2;
			curveVia: 0@0.1 to: -0.1@0.1;
			lineTo: -0.4@0;
			curveVia: -0.1@0 to: (-0.1@ -0.1);
			lineTo: 0@ -0.2  ;
			curveVia: 0@ -0.1 to: 0.1@ -0.1 ].

	aCanvas pathTransform scaleBy: self extent.
	aCanvas
		setPaint: self carColor;
		drawShape: path 
