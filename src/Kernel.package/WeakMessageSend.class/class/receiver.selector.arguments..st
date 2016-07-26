instance creation
receiver: anObject selector: aSymbol arguments: anArray
	^ self new
		receiver: anObject;
		selector: aSymbol;
		arguments: anArray
