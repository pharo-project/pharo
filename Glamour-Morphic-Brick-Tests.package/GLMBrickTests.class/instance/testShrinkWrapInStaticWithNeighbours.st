tests-layouter
testShrinkWrapInStaticWithNeighbours
	|root band child1 child2 neighbour childWidth width|
	width := 20.
	childWidth := self staticWidth - width.
	
	root := self newBrickWithStaticExtent useHorizontalLinearLayout.
	band := GLMBrick new useVerticalLinearLayout; hSpaceFill; vShrinkWrap.
	child1 := GLMBrick new height: self staticHeight; hSpaceFill.
	child2 := GLMBrick new height: self staticHeight; hSpaceFill.
	neighbour := GLMBrick new vSpaceFill; width: width.
	
	root addAllBricks: { band. neighbour }.
	band addAllBricks: { child1. child2 }.
	
	self assertAllSubbricksAreClean: root.
	
	self assert: root brickBounds equals: (0@0 extent: self staticExtent).
	self assert: neighbour brickBounds equals: (childWidth@0 extent: width@self staticHeight).
	self assert: child1 brickBounds equals: (0@0 extent: childWidth@self staticHeight).
	self assert: child2 brickBounds equals: (0@self staticHeight extent: childWidth@self staticHeight).
	self assert: band brickBounds equals: (0@0 extent: childWidth@(self staticHeight+self staticHeight)).