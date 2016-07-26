own services
serviceRenameFile

	^ (SimpleServiceEntry 
		provider: self 
		label: 'Rename' 
		selector: #renameFile 
		description: 'Rename file')