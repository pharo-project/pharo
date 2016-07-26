requirements
collectionWithCopy
	"return a collection of type 'self collectionWIithoutEqualsElements class' containing no elements equals ( with identity equality)
	but  2 elements only equals with classic equality"
	| result collection |
	collection := OrderedCollection withAll: self elementsCopyNonIdenticalWithoutEqualElements.
	collection add: collection first copy.
	result := self elementsCopyNonIdenticalWithoutEqualElements class withAll: collection.
	^ result