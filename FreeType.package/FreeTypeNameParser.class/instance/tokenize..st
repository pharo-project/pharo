parsing
tokenize: aString 
	"answer an OrderedCollection of {string. start. end} tuples.
	tokens are separated by $- $_ $, $. and whitespace"
	| currentTokens answer start |
	
	currentTokens := aString findTokens: delimiters keep: delimiters.
	answer := OrderedCollection new.
	start := 1.
	currentTokens do:[:tok |
		(delimiters includes: tok first)
			ifFalse:[answer add: {tok. start. start+tok size - 1}].
		start := start + tok size].
	^answer