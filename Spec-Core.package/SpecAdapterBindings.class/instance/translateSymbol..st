protocol
translateSymbol: aSymbol

	^ bindings 
		at: aSymbol
		ifAbsent: [ aSymbol ]