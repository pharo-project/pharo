Extension { #name : 'Class' }

{ #category : '*Epicea' }
Class >> asEpiceaRingDefinition [

	| ring |
	ring := (RGClassDefinition named: self name)
		        superclassName: self superclass asString;
		        traitCompositionSource: self traitCompositionString;
		        addInstanceVariables: self instVarNames;
		        addClassVariables: self classVarNames;
		        addSharedPools: self sharedPoolNames;
		        comment: self comment;
		        stamp: self commentStamp;
		        definitionSource: self definitionString;
		        package: self package name;
		        packageTag: self packageTag name;
		        withMetaclass.

	ring classSide
		traitCompositionSource: self classSide traitCompositionString;
		definitionSource: self classSide definitionString;
		addInstanceVariables: self classSide instVarNames.

	^ ring
]
