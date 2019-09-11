Extension { #name : #Class }

{ #category : #'*Monticello' }
Class >> asClassDefinition [
	"we use a very ugly hack to encode complex slots as string with MC... later MC should model Slots directly"
	self needsSlotClassDefinition ifFalse: [  
	 ^MCClassDefinition
		name: self name
		superclassName: (self superclass ifNil: [ nil asString ] ifNotNil: [ self superclass name ])
		traitComposition: self traitCompositionString
		classTraitComposition: self class traitCompositionString
		category: self category 
		instVarNames:  (self localSlots collect: #name)
		classVarNames: self classVarNames
		poolDictionaryNames: self sharedPoolNames
		classInstVarNames: (self class localSlots collect: #name)
		type: self typeOfClass
		comment: self organization classComment	asString
		commentStamp: self organization commentStamp].
	
	^MCClassDefinition
		name: self name
		superclassName: (self superclass ifNil: [ nil asString ] ifNotNil: [ self superclass name ])
		traitComposition: self traitCompositionString
		classTraitComposition: self class traitCompositionString
		category: self category 
		instVarNames: (self localSlots collect: #definitionString)
		classVarNames: (self classVariables collect: #definitionString)
		poolDictionaryNames: self sharedPoolNames
		classInstVarNames: (self class localSlots collect: #definitionString)
		type: self typeOfClass
		comment: self organization classComment	asString
		commentStamp: self organization commentStamp.
	




]

{ #category : #'*Monticello' }
Class >> classDefinitions [
	^ Array with: self asClassDefinition
]
