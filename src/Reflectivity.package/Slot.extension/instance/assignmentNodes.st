*Reflectivity
assignmentNodes
	^self astNodes 
		select: [ :each | each isWrite ] 
		thenCollect: [ :each | each parent ]