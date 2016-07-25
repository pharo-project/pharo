instance creation
for: aMethod selector: aSelector arguments: anArray
	^self new
		setMethod: aMethod;
		setKeyword: aSelector;
		setArguments: anArray;
		yourself