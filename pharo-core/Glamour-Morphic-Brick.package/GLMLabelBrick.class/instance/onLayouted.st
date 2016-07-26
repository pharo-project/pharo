brick-layout-events
onLayouted
	super onLayouted.
	
	self label isFitsInParent
		ifTrue: [ self isDotsAdded
			ifTrue: [
				self privateRemoveBrick: self dotsBrick.
				self doLayoutForce.
				self changed ] ]
		ifFalse: [ self isDotsAdded
			ifFalse: [
				self privateAddBrickBack: self dotsBrick.
				self dotsBrick wrappedBounds height: self outerBounds height.
				self layoutPolicy layoutFloating: self dotsBrick in: self brickBounds.
				self doLayoutForce.
				self changed ] ]
	
	