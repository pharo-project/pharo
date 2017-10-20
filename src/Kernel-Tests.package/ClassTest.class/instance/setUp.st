running
setUp
	className := #TUTU.
	self deleteClass.
	Object subclass: className
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self categoryNameForTemporaryClasses