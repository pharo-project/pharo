Extension { #name : 'Class' }

{ #category : '*Epicea' }
Class >> asEpiceaRingDefinition [

	| ring |
	ring := (RGClassDefinition named: self name)
		        category: self category;
		        superclassName: self superclass asString;
		        "Note it's nil for ProtoObject"traitCompositionSource:
			        self traitCompositionString;
		        addInstanceVariables: self instVarNames;
		        addClassVariables: self classVarNames;
		        addSharedPools: self sharedPoolNames;
		        comment: self comment;
		        stamp: self commentStamp;
		        definitionSource: self definitionString;
		        package: self packageName;
		        withMetaclass.

	ring classSide
		traitCompositionSource: self classSide traitCompositionString;
		definitionSource: self classSide definitionString;
		addInstanceVariables: self classSide instVarNames.

	^ ring
]
