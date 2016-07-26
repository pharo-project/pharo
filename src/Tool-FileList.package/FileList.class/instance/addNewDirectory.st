own services
addNewDirectory
	self 
		addNew: 'Directory'
		byEvaluating: [:newName | 
			(self directory / newName ) ensureCreateDirectory ]
