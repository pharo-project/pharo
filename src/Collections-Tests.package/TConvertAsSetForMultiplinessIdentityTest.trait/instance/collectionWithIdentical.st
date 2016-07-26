requirements
collectionWithIdentical
	"return a collection of type : 'self collectionWIithoutEqualsElements class containing two elements equals ( with identity equality)"
	| result collection anElement |
	collection := OrderedCollection withAll: self elementsCopyNonIdenticalWithoutEqualElements.
	anElement := collection first.
	collection add: anElement.
	result := self elementsCopyNonIdenticalWithoutEqualElements class withAll: collection.
	^ result