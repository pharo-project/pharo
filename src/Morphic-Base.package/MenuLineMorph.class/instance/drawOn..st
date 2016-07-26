drawing
drawOn: aCanvas 
	self theme flatMenu 
		ifFalse: [
			aCanvas
				fillRectangle: (bounds topLeft corner: bounds rightCenter)
				color: self baseColor twiceDarker .
			aCanvas
				fillRectangle: (bounds leftCenter corner: bounds bottomRight)
				color: self baseColor twiceLighter ]
		ifTrue: [
			aCanvas
				fillRectangle: (bounds topLeft corner: bounds bottomRight)
				color: self baseColor ]