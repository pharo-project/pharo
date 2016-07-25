*System-FileRegistry
serviceExtractAll
	"Answer a service for extracting files in a zip to a user-specified directory"

	^ FileModifyingSimpleServiceEntry 
		provider: self
		label: 'extract all to...'
		selector: #extractAllIn: 
		description: 'extract all files to a user-specified directory'
		buttonLabel: 'extract all'