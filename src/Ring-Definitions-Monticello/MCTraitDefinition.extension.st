Extension { #name : 'MCTraitDefinition' }

{ #category : '*Ring-Definitions-Monticello' }
MCTraitDefinition >> asRingDefinition [

	| ring |
	ring := (RGTraitDefinition named: self className)
		        package: (RGPackageDefinition named: self packageName);
		        packageTag: self tagName;
		        superclassName: self superclassName;
		        traitCompositionSource: self traitCompositionString;
		        comment: self comment;
		        stamp: self commentStamp;
		        definitionSource: self definitionString;
		        withMetaclass.

	ring classSide
		traitCompositionSource: self classTraitCompositionString;
		definitionSource: self classDefinitionString.
	^ ring
]
