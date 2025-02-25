Extension { #name : 'Class' }

{ #category : '*Monticello-Model' }
Class >> asClassDefinition [
	"we use a very ugly hack to encode complex slots as string with MC... later MC should model Slots directly"

	"We do not dispatch on the printer because in fact MC only support the oldPharo syntax. 
	This syntax is hijacked to store the full definition of a slot in case of complex slots: 
				#x => PropertySlot 
				
	This is produces by definitionString on slot while x is produced simnply by invoking name.
	"

	| definition |
	definition := (MCClassDefinition named: self name)
		              superclassName: (self superclass ifNotNil: [ self superclass name ]);
		              traitComposition: self traitCompositionString;
		              classTraitComposition: self class traitCompositionString;
		              packageName: self package name;
		              poolDictionaryNames: self sharedPoolNames;
		              type: self mcType;
		              comment: self comment;
		              commentStamp: self commentStamp;
		              yourself.

	"We do not export the tag if this is the root."
	self packageTag ifNotNil: [ :tag | tag isRoot ifFalse: [ definition tagName: tag name ] ].


	self needsSlotClassDefinition
		ifTrue: [
			definition
				instVarNames: (self localSlots collect: [ :each | each definitionString ]);
				classVarNames: (self classVariables collect: [ :each | each definitionString ]);
				classInstVarNames: (self class localSlots collect: [ :each | each definitionString ]) ]
		ifFalse: [
			definition
				instVarNames: (self localSlots collect: [ :each | each name ]);
				classVarNames: self classVarNames;
				classInstVarNames: (self class localSlots collect: [ :each | each name ]) ].

	^ definition
]
