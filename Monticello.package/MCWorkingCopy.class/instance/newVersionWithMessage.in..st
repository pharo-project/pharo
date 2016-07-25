operations
newVersionWithMessage: aMessageString in: aRepository
	^ self 
		newVersionWithName: (self uniqueVersionNameIn: aRepository)  
		message: aMessageString
		in: aRepository.

	"^ (self requestVersionNameAndMessageWithSuggestion: self uniqueVersionName) ifNotNil:
		[:pair |
		self newVersionWithName: pair first message: aMessageString].
	"