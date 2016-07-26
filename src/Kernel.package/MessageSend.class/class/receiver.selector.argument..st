instance creation
receiver: anObject selector: aSymbol argument: aParameter
	^ self receiver: anObject selector: aSymbol arguments: (Array with: aParameter)