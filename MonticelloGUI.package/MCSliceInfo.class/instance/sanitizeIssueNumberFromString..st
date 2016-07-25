input sanitizing
sanitizeIssueNumberFromString: aString
	^ aString select: [ :c | c isDigit ].