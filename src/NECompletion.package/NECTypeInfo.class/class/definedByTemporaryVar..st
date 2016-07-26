instance creation
definedByTemporaryVar: anInteger
	| newInstance |
	newInstance := self new.
	newInstance setType: nil kind: 3.
	newInstance temporaryOffset: anInteger.
	^ newInstance