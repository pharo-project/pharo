testing
isNullable

	^branch isNullable or: [regex notNil and: [regex isNullable]]