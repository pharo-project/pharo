accessing
stringHashesAt: aSymbol
	^ self 
		infoAt: aSymbol
		ifPresent: [ :array | array first ]
		ifAbsent: [ #() ]