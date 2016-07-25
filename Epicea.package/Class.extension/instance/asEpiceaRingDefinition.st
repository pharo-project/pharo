*Epicea-Ring
asEpiceaRingDefinition
	
	| ring |
	
	ring := (RGClassDefinition named: self name)
		category: self category;
		superclassName: self superclass name;  
		traitCompositionSource: self traitCompositionString;
		addInstanceVariables: self instVarNames;
		addClassVariables: self classVarNames;
		addSharedPools: self sharedPoolNames;
		comment: self organization classComment;
		stamp: self organization commentStamp;
		definitionSource: self definition;
		package: ((RPackage organizer packageOfClassNamed: self name) ifNotNil:#name);  "FIX: this is a workaround for class modifications... check if it's still necessary."
		withMetaclass.

	ring theMetaClass 
		traitCompositionSource: self theMetaClass traitCompositionString;
		definitionSource: self theMetaClass definition;
		addInstanceVariables: self theMetaClass instVarNames.  

	^ ring