accessing structure variables
texture_formats: anObject
	"This method was automatically generated"
	handle replaceFrom: OFFSET_TEXTURE_FORMATS to: OFFSET_TEXTURE_FORMATS + 63 with: anObject getHandle startingAt: 1