tests-geometry
testMinHeightOneStatic
	|brick|
	
	brick := self newBrickWithSmallStaticExtent.
	brick minHeight: self staticHeight.
	brick doLayoutForce.
	self assertAllSubbricksAreClean: brick.
	
	self assert: brick brickBounds equals: (0@0 extent: self staticSmallWidth @ self staticHeight)