drawing
drawOn: aCanvas

	super drawOn: aCanvas.
	aCanvas 
		drawString: self label
		in: self labelBounds
		font: self font
		color: self fontColor.
		
	
