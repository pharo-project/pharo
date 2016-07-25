morphic protocol
searchUpdate: aString

	pattern := '.' asRegexIgnoringCase.
	
	aString isEmptyOrNil ifFalse: [
		pattern := [aString asRegexIgnoringCase] on: RegexSyntaxError do: [ aString ]].
	
	self updateList.