pixel accessing
colorAt: aPoint put: aColor
	"Store the given color into the pixel at aPoint. The given color must match one of the colors in the receiver's colormap."

	| i |
	i := self colors indexOf: aColor
		ifAbsent: [^ self error: 'trying to use a color that is not in my colormap'].
	self pixelValueAt: aPoint put: i - 1.
