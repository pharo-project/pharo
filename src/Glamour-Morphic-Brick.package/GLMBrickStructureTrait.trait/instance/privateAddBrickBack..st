private-structure
privateAddBrickBack: aBrick
	|brick|
	
	brick := aBrick asBrick.
	self privateAddBrickLinkBack: brick.
	self privateSubbricks: (self subbricks copyWith: brick).
	
	^ aBrick