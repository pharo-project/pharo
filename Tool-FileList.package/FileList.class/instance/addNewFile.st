file menu action
addNewFile
	self 
		addNew: 'File'
		byEvaluating: [:newName | (self directory / newName) ensureCreateFile ]
