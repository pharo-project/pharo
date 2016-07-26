initialize local repository
validate
	"TODO: Validate that the selected local directory is a compatible git repository."
	self directory: (subdirectory isEmptyOrNil
		ifTrue: [ self localRepository  ]
		ifFalse: [ self localRepository / subdirectory ])