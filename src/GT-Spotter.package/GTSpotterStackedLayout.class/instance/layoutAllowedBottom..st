as yet unclassified
layoutAllowedBottom: aBrick

	self
		translate: aBrick by: (self deltaBottom: aBrick) negated;
		increaseZindex: aBrick;
		cutAllowedBoundsBottom: aBrick