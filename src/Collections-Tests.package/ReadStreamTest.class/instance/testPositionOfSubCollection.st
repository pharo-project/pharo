tests
testPositionOfSubCollection
	
	self assert: ('xyz' readStream positionOfSubCollection: 'q' ) = 0.
	self assert: ('xyz' readStream positionOfSubCollection: 'x' ) = 1.

	self assert: ('xyz' readStream positionOfSubCollection: 'y' ) = 2.
	self assert: ('xyz' readStream positionOfSubCollection: 'z' ) = 3.