testing
testInitialization

	self assert: topic title = 'Unnamed Topic'.
	self assert: topic key isEmpty.
	self assert: topic contents isEmpty 