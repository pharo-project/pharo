drawing
clearSurface
	renderer athensSurface
		drawDuring: [ :canvas | 
			canvas
				setPaint: Color white;
				drawShape: (0 @ 0 extent: window extent) ]