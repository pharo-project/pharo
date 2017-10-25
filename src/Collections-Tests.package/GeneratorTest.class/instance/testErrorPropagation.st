tests
testErrorPropagation
	"Ensure that errors in the generator block are properly propagated"

	| generator |
	generator := Generator
		on: [ :g | 
			g yield: 1.
			g error: 'yo' ].
	self should: [ generator next ] raise: Error