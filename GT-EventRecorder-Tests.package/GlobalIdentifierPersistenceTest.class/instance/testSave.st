tests
testSave
	| values |
	values := Dictionary new.
	values at: #a put: 42.
	persistence save: values.
	self assert: preferences exists.