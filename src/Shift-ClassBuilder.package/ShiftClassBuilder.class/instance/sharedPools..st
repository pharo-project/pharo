accessing
sharedPools: aString
	self layoutDefinition sharedPools: ((aString substrings: ' ') collect: #asSymbol)