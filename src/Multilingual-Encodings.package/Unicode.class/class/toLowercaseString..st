casing
toLowercaseString: aWideString
	"Transform a Wide String into lowercase.
	This does not handle special cases where number of characters could change.
	The algorithm would work for ByteString, however it's far from the most efficient."
	
	^aWideString collect: [:e |
		(ToLower at: e charCode ifAbsent: [nil])
			ifNil: [e]
			ifNotNil: [:low | self value: low]]