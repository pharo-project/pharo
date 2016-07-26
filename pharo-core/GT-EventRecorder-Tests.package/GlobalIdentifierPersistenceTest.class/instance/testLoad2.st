tests
testLoad2
	"Load stored values."
	| values loaded |
	values := Dictionary new.
	values at: #a put: 42.
	persistence save: values.
	loaded := Dictionary new.
	persistence load: loaded.
	self assert: loaded equals: values.
