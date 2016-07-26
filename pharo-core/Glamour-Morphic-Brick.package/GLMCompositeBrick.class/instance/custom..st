phlow
custom: aBrick
	self flag: 'for now we only allow one subbrick in the composite.
	We have to revisit this implementation after the composite will allow for multiple subbricks'.
	self hasSubbricks ifFalse: [ self addBrickBack: aBrick ].
	^ aBrick