tests
testCircular
	"Circular datastructures cannot be encoded using JSON"
	self
		should: [ | foo |
			foo := { 'foo'->'dummy' } asDictionary.
			foo at: 'bar' put: foo.
			STONJSON toString: foo ]
		raise: STONWriterError