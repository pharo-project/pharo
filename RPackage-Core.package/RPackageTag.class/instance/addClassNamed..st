accessing
addClassNamed: aSymbol 
	(classNames includes: aSymbol) ifTrue: [ ^ self ].
	^ classNames add: aSymbol.