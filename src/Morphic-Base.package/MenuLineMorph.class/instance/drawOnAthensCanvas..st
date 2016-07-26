drawing
drawOnAthensCanvas: anAthensCanvas
	self theme flatMenu
		ifFalse: [ 
			anAthensCanvas setPaint: self baseColor twiceDarker.
			anAthensCanvas drawShape: (bounds topLeft corner: bounds rightCenter).
			anAthensCanvas setPaint: self baseColor twiceLighter.
			anAthensCanvas drawShape: (bounds leftCenter corner: bounds bottomRight) ]
		ifTrue: [ 
			anAthensCanvas setPaint: self baseColor.
			anAthensCanvas drawShape: (bounds topLeft corner: bounds bottomRight) ]