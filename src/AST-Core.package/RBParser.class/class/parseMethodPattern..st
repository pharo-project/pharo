parsing
parseMethodPattern: aString 
	| parser |
	parser := self new.
	parser errorBlock: [:error :position | ^nil].
	parser initializeParserWith: aString.
	^parser parseMessagePattern selector