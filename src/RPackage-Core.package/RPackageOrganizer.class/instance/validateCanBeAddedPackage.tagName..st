private
validateCanBeAddedPackage: aPackage tagName: aSymbol
	aPackage name = aSymbol ifTrue: [ ^ self ].
	self 
		validateCanBeAddedPackageName: aPackage categoryName, '-', aSymbol
		tagName: nil