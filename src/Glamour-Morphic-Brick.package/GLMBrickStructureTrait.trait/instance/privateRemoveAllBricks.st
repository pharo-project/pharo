private-structure
privateRemoveAllBricks
	
	self hasSubbricks ifFalse: [ ^ self ].
	self subbricks do: [:each |
		each brickDoubleLink: nil.
		each owner: nil ].
	self subbricks removeAll