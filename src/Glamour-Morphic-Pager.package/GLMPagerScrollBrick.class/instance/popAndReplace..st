api
popAndReplace: aBrick
	|brick|

	brick := self configurePane: aBrick.
	brick wrappedBounds: self band lastSubbrick wrappedBounds.
	brick updateMorphBounds.
	
	self band doNotLayoutDuring: [ :b | b
		removeBrick: b lastSubbrick;
		addBrickBack: brick ].
	self changed
	
	