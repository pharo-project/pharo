own services
serviceAddNewDirectory
	"Answer a service entry characterizing the 'add new directory' command"

	^ SimpleServiceEntry 
		provider: self 
		label: 'Add new directory' 
		selector: #addNewDirectory
		description: 'Adds a new, empty directory (folder)' 