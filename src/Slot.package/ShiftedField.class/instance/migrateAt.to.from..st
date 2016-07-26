migrating
migrateAt: index to: newInstance from: oldInstance
	newInstance 
		instVarAt: index 
		put: (oldInstance instVarAt: index - shift)