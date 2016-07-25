style table
attributesFor: aSymbol pixelHeight: aNumber 
	^ (self textAttributesByPixelHeight
		at: aNumber
		ifAbsentPut: [self initialTextAttributesForPixelHeight: aNumber])
		at: aSymbol
		ifAbsent: [#()]