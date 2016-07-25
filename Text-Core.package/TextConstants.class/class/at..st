backward compatibility
at: aSymbol
	
	^ self classPool at: aSymbol ifAbsent: [self TextSharedInformation at: aSymbol]