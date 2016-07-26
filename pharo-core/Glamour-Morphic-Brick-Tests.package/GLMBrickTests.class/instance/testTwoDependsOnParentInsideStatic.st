tests-layouter
testTwoDependsOnParentInsideStatic
	|root brick1 brick2|
	
	root := self newBrickWithStaticExtent.
	brick1 := self newBrickFullHeightAndWidthDependsOnParent: 50.
	brick2 := self newBrickFullHeightAndWidthDependsOnParent: 50.
	
	root
		addBrickBack: brick1;
		addBrickBack: brick2.

	self assertAllSubbricksAreClean: root.
	
	self assert: root extent equals: self staticExtent.
	self assert: brick1 brickBounds equals: (0@0 extent: (self staticWidth / 2) @ self staticHeight).
	self assert: brick2 brickBounds equals: ((self staticWidth / 2)@0 extent: (self staticWidth / 2) @ self staticHeight).
	