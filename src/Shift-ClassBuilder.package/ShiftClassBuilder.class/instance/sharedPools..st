accessing
sharedPools: aString
	self layoutDefinition sharedPools: ((aString substrings: ' ') collect: [:e | e asSymbol])