instance creation
name: nameString
superclassName: superclassString
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
					traitComposition: '{}'
					classTraitComposition: '{}'
					category: categoryString 
					instVarNames: ivarArray
					classVarNames: cvarArray
					poolDictionaryNames: poolArray
					classInstVarNames: civarArray
					type: typeSymbol
					comment: commentString
					commentStamp: stampString)