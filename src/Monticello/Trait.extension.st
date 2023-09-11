Extension { #name : #Trait }

{ #category : #'*Monticello' }
Trait >> asClassDefinition [
	"We do not dispatch on the printer because in fact MC only support the oldPharo syntax. 
	This syntax is hijacked to store the full definition of a slot in case of complex slots: 
				#x => PropertySlot 
				
	This is produces by definitionString on slot while x is produced simnply by invoking name.
	"

	| definition |
	definition := self needsSlotClassDefinition
		              ifTrue: [
			              (MCTraitDefinition
				               name: self name
				               traitComposition: self traitCompositionString
				               category: self category
				               instVarNames: (self localSlots collect: [ :each | each definitionString ]))
				              classInstVarNames: (self classSide localSlots collect: [ :each | each definitionString ]);
				              yourself ]
		              ifFalse: [
			              (MCTraitDefinition
				               name: self name
				               traitComposition: self traitCompositionString
				               category: self category
				               instVarNames: (self localSlots collect: [ :each | each name ]))
				              classInstVarNames: (self class localSlots collect: [ :each | each name ]);
				              yourself ].

	definition
		classTraitComposition: self class traitCompositionString;
		comment: self comment;
		commentStamp: self commentStamp.

	^ definition
]

{ #category : #'*Monticello' }
Trait >> classDefinitions [
	| definitions |
	definitions := OrderedCollection with: self asClassDefinition.
	(self hasClassSide
		and: [self classTrait hasTraitComposition])
					ifTrue: [definitions add: self classTrait asMCDefinition].
	^definitions asArray
]
