casing
toLowercase: aCharacter
	"Transform a Wide String into lowercase.
	This does not handle special cases where number of characters could change.
	The algorithm would work for ByteString, however it's far from the most efficient."
	
	(ToLower at: aCharacter charCode ifAbsent: [ ^ aCharacter ])
			ifNotNil: [:low | ^ self value: low ]