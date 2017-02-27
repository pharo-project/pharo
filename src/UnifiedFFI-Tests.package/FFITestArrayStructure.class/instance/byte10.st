accessing structure variables
byte10
	"This method was automatically generated"
	^(FFITypeArray ofType: #FFIUInt8 size: 10) fromHandle: (handle copyFrom: OFFSET_BYTE10 to: OFFSET_BYTE10 + 9)