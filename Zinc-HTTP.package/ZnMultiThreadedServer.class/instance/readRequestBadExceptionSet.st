private
readRequestBadExceptionSet
	"Return the set of exceptions which, when they occur while reading a request,
	are interpreted as equivalent to a request parse error or bad request."

	^ self
		exceptionSet:
			#(#ZnParseError #ZnCharacterEncodingError #ZnUnknownScheme #ZnPortNotANumber #ZnTooManyDictionaryEntries #ZnEntityTooLarge)