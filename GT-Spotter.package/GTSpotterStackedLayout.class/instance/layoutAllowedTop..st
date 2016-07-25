as yet unclassified
layoutAllowedTop: aBrick

	self
		translate: aBrick by: (self deltaTop: aBrick);
		increaseZindex: aBrick;
		cutAllowedBoundsTop: aBrick