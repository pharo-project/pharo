tests - converting
testindexOfstartingAt

	self assert: ('aa aa éé aa aa' indexOf: $a startingAt: 8) = 10.
	self assert: ('aa aa éé aa aa' indexOf: $z startingAt: 8) = 0.