Extension { #name : 'Class' }

{ #category : '*Ring-Definitions-Core' }
Class >> asFullRingDefinition [
	"A behavior is converted to a ring class including its variables, methods, direct superclass, direct subclasses and the package in which is loaded.
	Active methods are generated and each knows its package as well."

	| rgClass |
	rgClass := self asRingDefinition.

	self superclass
		ifNotNil: [ rgClass superclass: self superclass asRingDefinition ]
		ifNil: [ self isTrait ifTrue: [ rgClass superclass: Trait asRingDefinition ] ].

	self subclasses do: [ :subclass | subclass asRingDefinition superclass: rgClass ].

	self methodsDo: [ :method | method asFullRingDefinitionInRGClass: rgClass ].
	self classSide methodsDo: [ :method | method asFullRingDefinitionInRGClass: rgClass classSide ].

	^ rgClass
]

{ #category : '*Ring-Definitions-Core' }
Class >> asRingDefinition [
	"A behavior is converted to a ring class. Only the receiver and its variables (instance, class, pools) are converted.
	Methods, superclasses, subclasses are not generated"

	| ring |
	ring := (RGClassDefinition named: self name)
		        superclassName: (self superclass
				         ifNil: [ nil printString ]
				         ifNotNil: [ self superclass name ]);
		        traitCompositionSource: self traitCompositionString;
		        addInstanceVariables: self instVarNames;
		        addClassVariables: self classVarNames;
		        addSharedPools: self sharedPoolNames;
		        comment: self comment;
		        stamp: self commentStamp;
		        definitionSource: self definitionString;
		        package: self package asRingDefinition;
		        packageTag: self packageTag name;
		        withMetaclass.

	ring classSide
		traitCompositionSource: self classSide traitCompositionString;
		definitionSource: self classSide definitionString;
		addInstanceVariables: self classSide instVarNames.
	^ ring
]
