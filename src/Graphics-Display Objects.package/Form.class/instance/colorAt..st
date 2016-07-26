pixel access
colorAt: aPoint
	"Return the color in the pixel at the given point.  "

	^ Color 
		colorFromPixelValue: (self pixelValueAt: aPoint)
		depth: self depth
