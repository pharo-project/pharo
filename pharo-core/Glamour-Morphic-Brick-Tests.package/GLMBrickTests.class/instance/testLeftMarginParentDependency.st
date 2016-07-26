tests-geometry
testLeftMarginParentDependency
	|root margin brick|
	margin := 10.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickFullHeightAndWidthDependsOnParent: 100.
	brick marginLeft: margin.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: (self staticExtent - (margin@0)).
	self assert: brick brickBounds equals: (margin@0 corner: self staticExtent)