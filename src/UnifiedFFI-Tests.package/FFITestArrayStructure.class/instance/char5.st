accessing structure variables
char5
	"This method was automatically generated"
	^(FFITypeArray ofType: #FFICharacterType size: 5) fromHandle: (handle copyFrom: OFFSET_CHAR5 to: OFFSET_CHAR5 + 4)