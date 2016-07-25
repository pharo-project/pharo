own services
serviceDeleteFile

	^ (SimpleServiceEntry 
		provider: self 
		label: 'Delete' 
		selector: #deleteFile)
		description: 'Delete the seleted item'