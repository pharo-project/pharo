Extension { #name : #MCClassDefinition }

{ #category : #'*Ring-Definitions-Monticello' }
MCClassDefinition >> asRingDefinition [
	| ring |

	ring:= (RGClassDefinition named:  self className)
		category: self category;
		superclassName: self superclassName;  
		traitCompositionSource: self traitCompositionString;
		addInstanceVariables: self instVarNames;
		addClassVariables: self classVarNames;
		addSharedPools: self poolDictionaries;
		comment: self comment;
		stamp: self commentStamp;
		definitionSource: self definitionString;
		withMetaclass.
	ring classSide 
		traitCompositionSource: self classTraitCompositionString;
		addInstanceVariables: self classInstVarNames;
		definitionSource: self classDefinitionString.
	^ring
]
