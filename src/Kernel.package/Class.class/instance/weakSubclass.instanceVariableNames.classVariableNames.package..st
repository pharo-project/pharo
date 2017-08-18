subclass creation - weak
weakSubclass: className instanceVariableNames: instVarNames 
	classVariableNames: classVarNames package: cat
	"Added to allow for a simplified subclass creation experience. "
	
	^ self weakSubclass: className 
		instanceVariableNames: instVarNames 
		classVariableNames: classVarNames 
		poolDictionaries: ''
		category: cat