running
setUp
	className := #TUTU.
	renamedName := #RenamedTUTU.
	self deleteClass.
	self deleteRenamedClass.
	Object subclass: className
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self categoryNameForTemporaryClasses