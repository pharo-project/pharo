*Ring-Core-Kernel
asRingDefinition
	"A behavior is converted to a ring class. Only the receiver and its variables (instance, class, pools) are converted.
	Methods, superclasses, subclasses are not generated"
	
	| ring |
	ring := (RGClassDefinition named: self name)
		category: self category;
		superclassName: (self superclass
			ifNil: [ nil printString ]
			ifNotNil: [ self superclass name ]);
		traitCompositionSource: self traitCompositionString;
		addInstanceVariables: self instVarNames;
		addClassVariables: self classVarNames;
		addSharedPools: self sharedPoolNames;
		comment: self organization classComment;
		stamp: self organization commentStamp;
		definitionSource: self definition;
		package: self package asRingDefinition;
		withMetaclass.
	ring theMetaClass 
		traitCompositionSource: self theMetaClass traitCompositionString;
		definitionSource: self theMetaClass definition;
		addInstanceVariables: self theMetaClass instVarNames.  
	^ ring