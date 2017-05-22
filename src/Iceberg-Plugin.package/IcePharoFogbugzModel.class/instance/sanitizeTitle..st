private
sanitizeTitle: aString
	| separators safeSeparator |
	
	aString isEmptyOrNil ifTrue: [ ^ self class unknownTitle ].
	
	safeSeparator := $-.
	separators := { Character space. Character tab. $_. safeSeparator }.
	^ String streamContents: [ :str | 
		aString do: [ :c |
			(separators includes: c) ifTrue: [ str nextPut: safeSeparator ].
			c isAlphaNumeric ifTrue: [ str nextPut: c ] ] ]