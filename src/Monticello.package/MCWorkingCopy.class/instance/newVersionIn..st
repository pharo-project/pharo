operations
newVersionIn: aRepository
	^ (self requestVersionNameAndMessageWithSuggestion: (self uniqueVersionNameIn: aRepository)) 
		ifNotNil: [:pair |
			self 
				newVersionWithName: pair first trimBoth 
				message: pair last 
				in: aRepository ].
