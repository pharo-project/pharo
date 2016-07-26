drawing-private
drawText: aStringOrText on: aCanvas in: aRectangle
	^ self 
		drawText: aStringOrText 
		on: aCanvas 
		in: aRectangle 
		color: self stringColorToUse.