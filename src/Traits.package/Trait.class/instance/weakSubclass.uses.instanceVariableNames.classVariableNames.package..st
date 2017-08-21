subclass creation - weak
weakSubclass: className uses: aTraitCompositionOrArray instanceVariableNames: instVarNames 
	classVariableNames: classVarNames package: cat
	"Added to allow for a simplified subclass creation experience. "
	
	^self weakSubclass: className uses: aTraitCompositionOrArray instanceVariableNames: instVarNames classVariableNames: classVarNames category: cat