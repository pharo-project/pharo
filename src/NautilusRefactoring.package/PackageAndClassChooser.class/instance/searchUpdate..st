updating
searchUpdate: aString
	searchPattern := nil.
	
	aString isEmptyOrNil ifFalse: [
		searchPattern := [aString asRegexIgnoringCase] on: RegexSyntaxError do: [ aString ]].
	
	self update.