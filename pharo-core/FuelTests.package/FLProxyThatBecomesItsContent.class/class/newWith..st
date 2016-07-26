instance creation
newWith: aByteSymbol 
	
	^self basicNew
		initializeWith: aByteSymbol;
		yourself