conversion
stringAsObject: aString
	"Answer the given string in object form."

	|transformed|
	transformed := self stringTransformBlock value: aString.
	^(transformed notNil and: [self needsConversion])
		ifTrue: [self objectClass readFromString: transformed]
		ifFalse: [transformed]