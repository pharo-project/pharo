drawing
clearSurface
	renderer athensSurface
		drawDuring:
			[ :canvas | 
			canvas setPaint: self backgroundColor.
			canvas drawShape: (0 @ 0 extent: self defaultExtent) ]