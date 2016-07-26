asserting
assertCollection: actual equals: expected
	"Specialized test method that generates a proper error message for collection"
	^ self
		assert: expected = actual
		description: [ self comparingCollectionBetween: actual and: expected ]