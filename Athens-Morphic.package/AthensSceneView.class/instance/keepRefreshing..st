accessing
keepRefreshing: aBoolean
	keepRefreshing := aBoolean.
	
	keepRefreshing ifTrue: [  self startStepping ].