*Epicea-Ring
asEpiceaRingDefinition
	
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
		definitionSource: self definition;
		package: (EpPlatform current packageNameFor: self);
		withMetaclass.

	ring theMetaClass 
		traitCompositionSource: self theMetaClass traitCompositionString;
		definitionSource: self theMetaClass definition;
		addInstanceVariables: self theMetaClass instVarNames.  

	^ ring