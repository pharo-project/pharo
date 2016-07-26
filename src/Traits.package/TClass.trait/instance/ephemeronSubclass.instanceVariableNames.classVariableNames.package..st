subclass creation - weak
ephemeronSubclass: className instanceVariableNames: instVarNames 
	classVariableNames: classVarNames package: cat
	"Added to allow for a simplified subclass creation experience. "
	
	^ self ephemeronSubclass: className 
		instanceVariableNames: instVarNames 
		classVariableNames: classVarNames 
		poolDictionaries: ''
		category: cat