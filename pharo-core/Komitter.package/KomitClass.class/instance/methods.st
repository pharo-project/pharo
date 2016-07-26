protocol
methods

	^ methods ifNil: [ methods := (self operations 
		select: [ :each | each isMethodPatch ]
		thenCollect: [ :each | each koMethod ]) sorted ]