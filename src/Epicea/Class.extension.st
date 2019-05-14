Extension { #name : #Class }

{ #category : #'*Epicea' }
Class >> asEpiceaRingDefinition [
	
	| ring |
	ring := (RGClassDefinition named: self name)
		category: self category;
		superclassName: self superclass asString; "Note it's nil for ProtoObject"
		traitCompositionSource: self traitCompositionString;
		addInstanceVariables: self instVarNames;
		addClassVariables: self classVarNames;
		addSharedPools: self sharedPoolNames;
		comment: self organization classComment;
		stamp: self organization commentStamp;
		definitionSource: self oldDefinition;
		package: (EpPlatform current packageNameFor: self);
		withMetaclass.

	ring classSide 
		traitCompositionSource: self classSide traitCompositionString;
		definitionSource: self classSide oldDefinition;
		addInstanceVariables: self classSide instVarNames.  

	^ ring
]
