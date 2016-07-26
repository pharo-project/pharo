subclass creation - variableWord
variableWordSubclass: className instanceVariableNames: instVarNames
	classVariableNames: classVarNames category: cat
	"Added to allow for a simplified subclass creation experience. "
	 
	^ self variableWordSubclass: className
		instanceVariableNames: instVarNames  
		classVariableNames: classVarNames 
		poolDictionaries: ''
		package: cat