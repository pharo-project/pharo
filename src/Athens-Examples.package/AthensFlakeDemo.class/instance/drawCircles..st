as yet unclassified
drawCircles: canvas

	canvas pathTransform restoreAfter: [
		
		canvas pathTransform scaleBy: 0.5 .
		
		canvas 
			setPaint: Color black;
			setShape: self circle;
			draw.

		canvas
			pathTransform scaleBy: 0.8.
		canvas
			setPaint: Color white;
			draw.
		canvas 
			pathTransform scaleBy: 0.8.
		canvas
			setPaint: Color black;
			draw
	]