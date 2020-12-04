Extension { #name : #Class }

{ #category : #'*Monticello' }
Class >> asClassDefinition [
	"we use a very ugly hack to encode complex slots as string with MC... later MC should model Slots directly"
	"We do not dispatch on the printer because in fact MC only support the oldPharo syntax. 
	This syntax is hijacked to store the full definition of a slot in case of complex slots: 
				#x => PropertySlot 
				
	This is produces by definitionString on slot while x is produced simnply by invoking name.
	"
	^ self needsSlotClassDefinition 
		ifTrue: [ MCClassDefinition
						name: self name
						superclassName: (self superclass ifNil: [ nil asString ] ifNotNil: [ self superclass name ])
						traitComposition: self traitCompositionString
						classTraitComposition: self class traitCompositionString
						category: self category 
						instVarNames: (self localSlots collect: [:each | each definitionString])
						classVarNames: (self classVariables collect: [:each | each definitionString])
						poolDictionaryNames: self sharedPoolNames
						classInstVarNames: (self class localSlots collect: [:each | each definitionString])
						type: self typeOfClass
						comment: self organization classComment	asString
						commentStamp: self organization commentStamp ]
	
		ifFalse: [  
				 MCClassDefinition
						name: self name
						superclassName: (self superclass ifNil: [ nil asString ] ifNotNil: [ self superclass name ])
						traitComposition: self traitCompositionString
						classTraitComposition: self class traitCompositionString
						category: self category 
						instVarNames:  (self localSlots collect: [ :each | each name ])
						classVarNames: self classVarNames
						poolDictionaryNames: self sharedPoolNames
						classInstVarNames: (self class localSlots collect: [ :each | each name ])
						type: self typeOfClass
						comment: self organization classComment	asString
						commentStamp: self organization commentStamp ]
	





]

{ #category : #'*Monticello' }
Class >> classDefinitions [
	^ Array with: self asClassDefinition
]
