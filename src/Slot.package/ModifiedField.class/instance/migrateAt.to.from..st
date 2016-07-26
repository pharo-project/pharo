migrating
migrateAt: index to: newInstance from: oldInstance
	newInstance
		instVarAt: index
		put: (oldInstance instVarAt: self oldFieldIndex)