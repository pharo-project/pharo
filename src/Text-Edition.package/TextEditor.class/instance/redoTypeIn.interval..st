undoers-redoers
redoTypeIn: aText interval: anInterval
	self selectInterval: anInterval.
	self replace: self selectionInterval with: aText and:
		[self selectInterval: (anInterval first to: anInterval first + aText size - 1)].
