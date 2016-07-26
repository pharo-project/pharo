figures
figure8: aCanvas
	| pt |
	pt := aCanvas pathTransform.
	pt restoreAfter:[
		| stroke p  wheel decorator |
		pt scaleBy: self extent .
		self drawBackgroundOn: aCanvas.

		pt scaleBy: (frame/100) sin abs.
		self drawCarOn: aCanvas.
		pt scaleBy: 0.25@0.2.5. 
		self drawCarOn: aCanvas.
	
		pt  translateBy: 2@0; rotateByDegrees: 35.
		self drawCarOn: aCanvas.
	
	  ].
		