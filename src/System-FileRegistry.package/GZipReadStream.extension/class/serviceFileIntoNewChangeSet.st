*System-FileRegistry
serviceFileIntoNewChangeSet
	"Answer a service for filing in an entire file"
	^ SimpleServiceEntry
		provider: self
		label: 'Install into new change set'
		selector: #fileIntoNewChangeSet:
		description: 'Install the decompressed contents of the file as a body of code in the image: create a new change set and file-in the selected file into it'
		buttonLabel: 'Install'