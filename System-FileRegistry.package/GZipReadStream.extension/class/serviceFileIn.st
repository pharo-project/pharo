*System-FileRegistry
serviceFileIn
	"Answer a service for filing in an entire file"

	^ SimpleServiceEntry 
		provider: self 
		label: 'FileIn entire file'
		selector: #fileIn:
		description: 'File in the entire decompressed contents of the file, which is expected to contain Smalltalk code in fileout ("chunk") format'
		buttonLabel: 'Filein'

