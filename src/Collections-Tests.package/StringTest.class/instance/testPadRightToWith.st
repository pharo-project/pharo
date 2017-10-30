tests
testPadRightToWith
	self assert: ('a' padRightTo: 3 with: $#) equals: 'a##'. 
	self assert: ('a' padRightTo: 1 with: $#) equals: 'a'. 
	self assert: ('a' padRightTo: 0 with: $#) equals: 'a'.