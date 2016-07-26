accessing
stringSizesAt: aSymbol
	^ self 
		infoAt: aSymbol
		ifPresent: [ :array | array second ]
		ifAbsent: [ #() ]