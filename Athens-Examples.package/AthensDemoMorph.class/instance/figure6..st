figures
figure6: aCanvas
	| pt c linearGradient |
	
	self figure5: aCanvas.
	
	linearGradient := 
		aCanvas surface createLinearGradient: {
			0 -> self coloredCarColor .
			0.25 -> self coloredCarColor muchLighter.
			0.5 -> Color white.
			0.75 -> (self coloredCarColor muchLighter ).
			1 -> (self coloredCarColor alpha: 0)	
		}
		start:  0@100  
		stop: 0@300.
	
	aCanvas setShape: (self roundedRectanglePathOn: aCanvas).
	
	aCanvas setPaint: linearGradient.
	
	aCanvas draw.
		