conversion
objectAsString: anObject
	"Answer the given object in string form."

	^(self objectTransformBlock value: anObject)
		ifNotNil: [:o | o asString]