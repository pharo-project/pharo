variable accessing
addPoolDictionary: aString 
	self privatePoolDictionaryNames add: aString asSymbol.
	model addPool: aString to: self