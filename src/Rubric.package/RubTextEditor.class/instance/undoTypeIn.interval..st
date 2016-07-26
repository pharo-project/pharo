undoers-redoers
undoTypeIn: aText interval: anInterval
	self selectInterval: anInterval.
	self replace: anInterval with: aText and:
		[self selectInterval: (anInterval first to: anInterval first - 1)].
