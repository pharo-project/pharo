*Polymorph-Widgets
treeRenderOn: aCanvas bounds: drawBounds color: drawColor font: aFont from: aMorph

	aCanvas
		drawString: self
		in: drawBounds 
		font: aMorph font
		color: drawColor