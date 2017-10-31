asserting
assertCollection: actual includesAll: subcollection
	"Specialized test method that generates a proper error message for collection"
	^ self
		assert: (actual includesAll: subcollection)
		description: [ actual asString, ' does not include all in ', subcollection asString ]