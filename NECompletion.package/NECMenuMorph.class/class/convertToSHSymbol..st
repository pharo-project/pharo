preferences
convertToSHSymbol: aSymbol
	^ (SHTextStylerST80 new attributesFor: aSymbol) isNotEmpty
		ifTrue: [ aSymbol ]
		ifFalse: [ #default ]