public
format: aParseTree withIndents: anInteger 
	^ self new
		indent: anInteger;
		format: aParseTree