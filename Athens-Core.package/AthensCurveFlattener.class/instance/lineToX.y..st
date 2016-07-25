subdivision callbacks
lineToX: trX y: trY

	^ dest lineTo: (self inverseTransform: trX @ trY) 