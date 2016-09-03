accessing
pattern: aString
	super pattern: aString.
	
	regex := aString asRegexIgnoringCase