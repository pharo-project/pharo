*Ring-Monticello
asRingDefinition

	| ring |
	ring := ( RGTraitDefinition named: self className)
		category: self category;
		superclassName: self superclassName;  
		traitCompositionSource: self traitCompositionString;
		comment: self comment;
		stamp: self commentStamp;
		definitionSource: self definitionString;
		withMetaclass.
		
	ring theMetaClass 
		traitCompositionSource: self classTraitCompositionString;
		definitionSource: self classDefinitionString.
	^ring