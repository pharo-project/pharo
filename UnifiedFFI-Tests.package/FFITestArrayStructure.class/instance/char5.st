accessing structure variables
char5
	"This method was automatically generated"
	^(FFITypeArray ofType: #FFICharacterType size: 5) fromHandle: (handle copyFrom: 1 to: 5)