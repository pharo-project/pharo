*Morphic-Base-Widgets
listRenderOn: aCanvas atRow: aRow bounds: bounds color: color backgroundColor: backgroundColor from: aMorph

	aCanvas
		drawString: self
		in: bounds
		font: aMorph font
		color: color