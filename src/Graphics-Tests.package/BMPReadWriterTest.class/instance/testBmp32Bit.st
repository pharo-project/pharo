tests - reading
testBmp32Bit
	| reader form |
	reader := BMPReadWriter new on: self bmpData32bit readStream.
	form := reader nextImage.
	self assert: (form colorAt: 7 @ 1) = Color red.
	self assert: (form colorAt: 1 @ 7) = Color green.
	self assert: (form colorAt: 7 @ 7) = Color blue.
	self assert: (form colorAt: 4 @ 4) = Color white.
	self assert: (form pixelValueAt: 1 @ 1) = 4278190080