own services
serviceAddNewFile
	"Answer a service entry characterizing the 'add new file' command"

	^(SimpleServiceEntry 
		provider: self 
		label: 'Add new file' 
		selector: #addNewFile 
		description: 'Create a new,. empty file, and add it to the current directory.')
		usingLineAfter: false