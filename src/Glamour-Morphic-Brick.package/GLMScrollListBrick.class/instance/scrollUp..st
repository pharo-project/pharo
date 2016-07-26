scrolling
scrollUp: anInteger
	
	self scrollBy: (
		self isFirstAdded
			ifTrue: [ anInteger min: (self topOffset - self bandTopOffset) ]
			ifFalse: [ anInteger ])