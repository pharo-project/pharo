drawing
drawCircleAt: point color: color
	renderer athensSurface
		drawDuring: [ :canvas | 
			canvas pathTransform translateBy: point.
			canvas
				setPaint: color;
				setShape: circle;
				draw ]