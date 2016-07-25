private
isSchemeNotUsingDoubleSlash: schemeString
	^ self class schemesNotUsingDoubleSlash includes: schemeString asLowercase