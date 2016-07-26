instance creation
name: nameString
superclassName: superclassString
traitComposition: traitCompositionString
classTraitComposition: classTraitCompositionString
category: categoryString 
instVarNames: ivarArray
classVarNames: cvarArray
poolDictionaryNames: poolArray
classInstVarNames: civarArray
type: typeSymbol
comment: commentString
commentStamp: stampString
	
	^ self instanceLike:
		(self new initializeWithName: nameString
					superclassName: superclassString
					traitComposition: traitCompositionString
					classTraitComposition: classTraitCompositionString
					category: categoryString 
					instVarNames: ivarArray
					classVarNames: cvarArray
					poolDictionaryNames: poolArray
					classInstVarNames: civarArray
					type: typeSymbol
					comment: commentString
					commentStamp: stampString)