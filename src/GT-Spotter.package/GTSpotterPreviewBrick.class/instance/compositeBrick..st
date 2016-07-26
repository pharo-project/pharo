accessing
compositeBrick: aBrick

	self compositeBrick ifNotNil: [ self removeBrick: self compositeBrick ].
	compositeBrick := aBrick.
	self themer spotterThemer spotterPreviewCompositeStyleFor: self compositeBrick.
	self addBrickBack: self compositeBrick.
	self doLayoutForce