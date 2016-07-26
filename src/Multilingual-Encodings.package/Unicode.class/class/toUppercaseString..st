casing
toUppercaseString: aWideString
	"Transform a Wide String into uppercase.
	This does not handle special cases where number of characters could change.
	The algorithm would work for ByteString, however it's far from the most efficient."
	
	^aWideString collect: [:e |
		(ToUpper at: e charCode ifAbsent: [nil])
			ifNil: [e]
			ifNotNil: [:up | self value: up]]