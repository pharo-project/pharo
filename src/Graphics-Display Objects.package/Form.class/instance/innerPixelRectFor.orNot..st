analyzing
innerPixelRectFor: pv orNot: not
	"Return a rectangle describing the smallest part of me that includes 
	all pixels of value pv.
	Note:  If orNot is true, then produce a copy that includes all pixels
	that are DIFFERENT from the supplied (background) value"

	| xTally yTally |
	xTally := self xTallyPixelValue: pv orNot: not.
	yTally := self yTallyPixelValue: pv orNot: not.
	^ ((xTally findFirst: [:t | t>0]) - 1) @ ((yTally findFirst: [:t | t>0]) - 1)
		corner:
			(xTally findLast: [:t | t>0])@(yTally findLast: [:t | t>0])