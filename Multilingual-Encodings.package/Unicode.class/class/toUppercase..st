casing
toUppercase: aCharacter
	"Transform a Wide String into uppercase.
	This does not handle special cases where number of characters could change.
	The algorithm would work for ByteString, however it's far from the most efficient."
	
	(ToUpper at: aCharacter charCode ifAbsent: [ ^ aCharacter ])
			ifNotNil: [:up | ^ self value: up]