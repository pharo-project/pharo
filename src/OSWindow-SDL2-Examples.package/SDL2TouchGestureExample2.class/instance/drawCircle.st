drawing
drawCircle
	renderer athensSurface
		drawDuring: [ :canvas | 
			canvas pathTransform translateBy: circleCenter.
			canvas
				setPaint:
						(circleSize positive
								ifTrue: [ positiveColor ]
								ifFalse: [ negativeColor ]);
				setShape: (self generateCircleOfSize: circleSize);
				draw ]