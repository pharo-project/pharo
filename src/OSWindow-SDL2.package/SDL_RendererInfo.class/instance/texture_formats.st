accessing structure variables
texture_formats
	"This method was automatically generated"
	^(FFITypeArray ofType: #FFIUInt32 size: 16) fromHandle: (handle copyFrom: OFFSET_TEXTURE_FORMATS to: OFFSET_TEXTURE_FORMATS + 63)