testing
isNullable

	^piece isNullable and: [branch isNil or: [branch isNullable]]