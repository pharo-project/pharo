drawing
drawBlackBackground
	renderer athensSurface
		drawDuring: [ :canvas | 
			canvas setPaint: blackBackground.
			canvas drawShape: (blackBackgroundPosition @ 0 extent: window extent) ]