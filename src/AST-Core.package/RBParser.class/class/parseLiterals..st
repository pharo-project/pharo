parsing
parseLiterals: aString 
	| parser |
	parser := self new.
	parser initializeParserWith: aString.
	^parser parseLiterals: aString