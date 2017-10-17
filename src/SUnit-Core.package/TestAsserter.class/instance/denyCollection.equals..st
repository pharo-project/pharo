asserting
denyCollection: actual equals: expected
	"Specialized test method that generates a proper error message for collection"
	^ self
		deny: expected = actual
		description: [ self unexpectedEqualityStringBetween: actual and: expected ]