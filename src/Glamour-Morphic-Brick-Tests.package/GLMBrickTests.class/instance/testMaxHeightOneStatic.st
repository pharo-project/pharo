tests-geometry
testMaxHeightOneStatic
	|brick|
	
	brick := self newBrickWithStaticExtent.
	brick maxHeight: self staticSmallHeight.
	brick doLayoutForce.
	self assertAllSubbricksAreClean: brick.
	
	self assert: brick brickBounds equals: (0@0 extent: self staticWidth @ self staticSmallHeight)