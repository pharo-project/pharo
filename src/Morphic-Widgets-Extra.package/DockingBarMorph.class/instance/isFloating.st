testing
isFloating
	"Answer true if the receiver has a float layout"
	^ self isHorizontal not
		and: [self isVertical not]