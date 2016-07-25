tests-layouter
test100DependsOnChildrenBricksInsideOneStatic
	|root current|
	
	root := self newBrickWithFullDependsOnChildren.
	current := root.
	1 to: 99 do: [ :index |
		current := current addBrickBack: self newBrickWithFullDependsOnChildren ].
	current := current addBrickBack: self newBrickWithStaticExtent.
	
	self assertAllSubbricksAreClean: root.
	
	self assert: root extent equals: current extent
	