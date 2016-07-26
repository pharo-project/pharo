accessing
stringSpecialsAt: aSymbol
	^ self 
		infoAt: aSymbol
		ifPresent: [ :array | array third ]
		ifAbsent: [ #() ]