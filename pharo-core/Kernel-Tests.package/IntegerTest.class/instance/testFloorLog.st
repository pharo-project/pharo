tests - mathematical functions
testFloorLog
	self assert: (100 floorLog: 10) = 2.
	self assert: (((2 raisedTo: Float emax + 3) floorLog: 10) = (2 log*(Float emax + 3)) floor) description: 'Integer>>floorLog: should not overflow'