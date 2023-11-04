Extension { #name : 'Trait' }

{ #category : '*Ring-Definitions-Core' }
Trait >> asRingDefinition [
	"A triat is converted to a ring class. Only the receiver is converted.
	Methods, superclasses, subclasses are not generated"

	| ring |
	ring := (RGTraitDefinition named: self name)
		        category: self category;
		        superclassName: #Trait;
		        traitCompositionSource: self traitCompositionString;
		        comment: self comment;
		        stamp: self commentStamp;
		        definitionSource: self definitionString;
		        package: self package asRingDefinition;
		        withMetaclass.

	ring classSide
		traitCompositionSource: self classSide traitCompositionString;
		definitionSource: self classSide definitionString.

	^ ring
]
