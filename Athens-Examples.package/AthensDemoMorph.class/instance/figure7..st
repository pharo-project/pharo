figures
figure7: aCanvas
	| pt |

	self drawBackgroundOn: aCanvas.

	pt := aCanvas pathTransform.

	pt restoreAfter:[

		pt scaleBy: self extent .
		self drawCarOn: aCanvas.

		pt scaleBy: 0.25@0.25. 
		self drawCarOn: aCanvas.
	
		pt  translateBy: 2@0; rotateByDegrees: 35.
		self drawCarOn: aCanvas.
	].
		