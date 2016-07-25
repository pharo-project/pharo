action
narrowWith: aString 
	completionToken := aString.
	model ifNotNil: [ model narrowWith: aString ]