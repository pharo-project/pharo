multibyte character methods
hasGlyphOf: aCharacter
	| code |
	code := aCharacter charCode.
	(code between: self minAscii and: self maxAscii)
		ifFalse: [ ^ false ].
	characterToGlyphMap ifNotNil: [ :map | code := map at: code + 1 ].
	(xTable at: code + 1) < 0
		ifTrue: [ ^ false ].
	^ true