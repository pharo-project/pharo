layout
layout: aBrick in: newBounds

	self
		layout: aBrick
		in: newBounds
		inject: aBrick paddingLeft @ aBrick paddingTop
		into: [ :origin :each |
			each outerOrigin: origin.
			each outerBounds width@0 ]