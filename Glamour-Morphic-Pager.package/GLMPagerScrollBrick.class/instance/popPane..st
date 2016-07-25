api
popPane: isSmooth

	(isSmooth not or: [self lastPane isNil]) ifTrue: [ ^ self popPane ].
	
	self band doNotLayoutDuring: [ :b | b removeBrick: b lastSubbrick ].
	self
		scrollTo: (pagerModel convertIndexToValue: pagerModel firstVisiblePageIndex with: self band subbricksCount)
		smooth: isSmooth
		callback: [ self doLayoutForce ]