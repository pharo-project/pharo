instance creation
definedByLiteral: aClass 
	| newInstance |
	newInstance := self new.
	newInstance setType: aClass kind: 1.
	^ newInstance