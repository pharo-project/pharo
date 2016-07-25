testing
testSpeed
	self 
		should: [ 100000 timesRepeat: [ generator next ] ] 
		notTakeMoreThan: 1 second