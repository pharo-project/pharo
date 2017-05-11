tests
testLoad3
	"Load stored values."
	| values loaded |
	persistence checker: self newFalseChecker.
	values := Dictionary new.
	values at: #a put: 42.
	persistence save: values.
	loaded := Dictionary new.
	persistence load: loaded.
	self assert: loaded isEmpty.