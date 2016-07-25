casing
toCasefold: aWideString
	"Transform a Wide String into fold case.
	This is to enable case insensitive conversion."
	
	^aWideString collect: [:e |
		(ToCasefold at: e charCode ifAbsent: [nil])
			ifNil: [e]
			ifNotNil: [:low | self value: low]]