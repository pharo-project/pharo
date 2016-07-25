pixel accessing
colorAt: aPoint
	"Return the color of the pixel at aPoint."

	^ self colors at: (self pixelValueAt: aPoint) + 1
