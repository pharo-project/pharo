tests
testAsPackageIfAbsent
	self assert: ('___inexistent___package___' asPackageIfAbsent: [ true ])