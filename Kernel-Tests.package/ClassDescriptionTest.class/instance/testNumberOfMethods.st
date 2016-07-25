tests
testNumberOfMethods
	self assert: (Point numberOfMethods = (Point localMethods size + Point class localMethods size)).
