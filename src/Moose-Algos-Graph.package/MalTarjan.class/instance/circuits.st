accessing
circuits
	^ self stronglyConnectedComponents
		select: [ :each | each size > 1 ]