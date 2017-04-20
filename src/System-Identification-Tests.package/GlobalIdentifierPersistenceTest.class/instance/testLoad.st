tests
testLoad
	"Load when nothing is stored."
	| values |
	values := Dictionary new.
	values at: #a put: 42.
	persistence load: values.
	self assert: values size equals: 1.
	self assert: (values at: #a) equals: 42.
	self assert: preferences exists.