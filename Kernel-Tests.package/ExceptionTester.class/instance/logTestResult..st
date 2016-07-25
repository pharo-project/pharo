logging
logTestResult: aString

	| index |
	index := self suiteLog size.
	self suiteLog 
		at: index
		put: ((self suiteLog at: index), ' ', aString)