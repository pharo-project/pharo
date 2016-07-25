initialization
setField: fieldName toString: fieldValue
	^self setField: fieldName to: (MIMEHeaderValue forField: fieldName fromString: fieldValue)