initialization
setPattern: aString

	pattern := aString.
	lowercasePattern := pattern asLowercase.
	indexes := Array new: pattern size.
	
