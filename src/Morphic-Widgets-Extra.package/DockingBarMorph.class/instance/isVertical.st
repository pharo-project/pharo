testing
isVertical
	"Answer true if the receiver has a vertical layout"
	^ self isAdheringToLeft
		or: [self isAdheringToRight]
