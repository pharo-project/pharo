tests
testIsSeperator

	| matcher |
	
	matcher := self newMatcher.
	
	self 
		assert: 	(matcher isSeperator: $_);
		assert: 	(matcher isSeperator: $:);
		deny: 		(matcher isSeperator: $!);
		deny: 		(matcher isSeperator: $a)
