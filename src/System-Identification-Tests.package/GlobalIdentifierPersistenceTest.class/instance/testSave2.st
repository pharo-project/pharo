tests
testSave2
	| values |
	persistence checker: self newFalseChecker.
	values := Dictionary new.
	values at: #a put: 42.
	persistence save: values.
	self deny: preferences exists.