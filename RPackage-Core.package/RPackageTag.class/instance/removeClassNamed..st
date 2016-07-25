accessing
removeClassNamed: aSymbol 
	^ classNames remove: aSymbol ifAbsent: [].