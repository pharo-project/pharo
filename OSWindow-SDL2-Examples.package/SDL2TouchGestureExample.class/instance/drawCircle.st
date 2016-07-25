drawing
drawCircle
	renderer athensSurface
		drawDuring: [ :canvas | 
			canvas pathTransform translateBy: circleCenter.
			canvas
				setPaint:
						(circleSize positive
								ifTrue: [ Color red ]
								ifFalse: [ Color blue ]);
				setShape: (self generateCircleOfSize: circleSize);
				draw ]