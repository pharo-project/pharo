tests - instance creation
testANewRunArrayIsEmpty

	| t |
	t := RunArray new.
	self assert:t isEmpty 
