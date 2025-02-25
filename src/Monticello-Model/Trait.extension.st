Extension { #name : 'Trait' }

{ #category : '*Monticello-Model' }
Trait >> asClassDefinition [
	"We do not dispatch on the printer because in fact MC only support the oldPharo syntax. 
	This syntax is hijacked to store the full definition of a slot in case of complex slots: 
				#x => PropertySlot 
				
	This is produces by definitionString on slot while x is produced simnply by invoking name.
	"

	| definition |
	definition := (MCTraitDefinition named: self name)
		              traitComposition: self traitCompositionString;
		              packageName: self package name;
		              classTraitComposition: self class traitCompositionString;
		              comment: self comment;
		              commentStamp: self commentStamp.

	"We do not export the tag if this is the root."
	self packageTag ifNotNil: [ :tag | tag isRoot ifFalse: [ definition tagName: tag name ] ].

	self needsSlotClassDefinition
		ifTrue: [
			definition
				instVarNames: (self localSlots collect: [ :each | each definitionString ]);
				classInstVarNames: (self classSide localSlots collect: [ :each | each definitionString ]) ]
		ifFalse: [
			definition
				instVarNames: (self localSlots collect: [ :each | each name ]);
				classInstVarNames: (self class localSlots collect: [ :each | each name ]) ].

	^ definition
]
