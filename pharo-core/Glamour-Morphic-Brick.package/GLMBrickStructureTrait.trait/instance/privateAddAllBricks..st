private-structure
privateAddAllBricks: aCollection
	|bricks|
	bricks := aCollection collect: #asBrick.
	bricks do: [ :each | self privateAddBrickLinkBack: each ].
	self privateSubbricks: (self subbricks, bricks).
	^ bricks