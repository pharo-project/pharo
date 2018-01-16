Extension { #name : #RG2ClassDefinition }

{ #category : #'*Ring2-Monticello' }
RG2ClassDefinition >> asMCDefinition [

	^ MCClassDefinition 
		name: self name
		superclassName: (self superclass ifNil: [ 'nil' ] ifNotNil: [ :aSuperclass | aSuperclass name ])
		traitComposition: self traitCompositionString 
		classTraitComposition: self metaclass traitCompositionString
		category: self category 
		instVarNames: self instVarNames
		classVarNames: self classVarNames
		poolDictionaryNames: self sharedPoolNames
		classInstVarNames: self metaclass instVarNames
		type: self mcType
		comment: self comment content
		commentStamp: self comment stamp
]

{ #category : #'*Ring2-Monticello' }
RG2ClassDefinition >> classPool [

	"TODO"
	^ Dictionary newFrom: (self classVarNames collect: [ :each | each -> nil ])
]

{ #category : #'*Ring2-Monticello' }
RG2ClassDefinition >> mcType [

	^ self layout mcType
]
