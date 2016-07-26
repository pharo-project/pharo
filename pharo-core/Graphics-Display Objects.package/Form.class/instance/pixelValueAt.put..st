pixel access
pixelValueAt: aPoint put: pixelValue
	"Store the given raw pixel value at the given point. Typical clients use colorAt:put: to store a color. "

	(BitBlt bitPokerToForm: self) pixelAt: aPoint put: pixelValue.
