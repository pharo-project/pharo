tests
testPadRightTo
	self assert: ('a' padRightTo: 3) equals: 'a  '. 
	self assert: ('a' padRightTo: 1) equals: 'a'. 
	self assert: ('a' padRightTo: 0) equals: 'a'.