instance creation
definedByMessageSend: aClass 
	| newInstance |
	newInstance := self new.
	newInstance setType: aClass kind: 2.
	^ newInstance