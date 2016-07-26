*CodeExport
stringForFileOut

	^ String streamContents: [:aStream || elementIndex |
			elementIndex := 1.
			1 to: categoryArray size do: 
				[:i | 
				aStream nextPut: $(.
				(categoryArray at: i) asString printOn: aStream.
				[elementIndex <= (categoryStops at: i)]
					whileTrue: 
						[aStream space; nextPutAll: (elementArray at: elementIndex).
						elementIndex := elementIndex + 1].
				aStream nextPut: $); cr ]]