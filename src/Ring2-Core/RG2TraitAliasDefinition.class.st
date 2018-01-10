Class {
	#name : #RG2TraitAliasDefinition,
	#superclass : #RG2TraitTransformationDefinition,
	#instVars : [
		'aliases'
	],
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'accessing - backend' }
RG2TraitAliasDefinition >> addAlias: aSymbol [ 

	self parent parent announceDefinitionChangeDuring: [ 
		self backend forBehavior addAlias: aSymbol to: self ].
]

{ #category : #'queries - aliases' }
RG2TraitAliasDefinition >> aliases [

	| allAliases |
	allAliases := OrderedCollection new.
	self aliasesDo: [ :each | allAliases add: each].
	^ allAliases 

]

{ #category : #'accessing - backend' }
RG2TraitAliasDefinition >> aliasesDo: aBlock [

	self backend forBehavior aliasesFor: self do: aBlock
]

{ #category : #'accessing - backend' }
RG2TraitAliasDefinition >> cleanAliases [

	self parent parent announceDefinitionChangeDuring: [ 
		self backend forBehavior cleanAliasesFor: self ].
]

{ #category : #'default model values' }
RG2TraitAliasDefinition >> defaultAliases [

	^ OrderedCollection new
]

{ #category : #initialize }
RG2TraitAliasDefinition >> initialize [

	super initialize.
	
	aliases := self unresolvedValue: self defaultAliases.
]

{ #category : #initialize }
RG2TraitAliasDefinition >> initializeUnresolved [

	super initializeUnresolved.
	
	aliases := self unresolvedValue: self defaultAliases.
]

{ #category : #'testing types' }
RG2TraitAliasDefinition >> isTraitAlias [

	^ true
]

{ #category : #resolving }
RG2TraitAliasDefinition >> makeMinimallyResolved [

	self isRing2Resolved ifTrue: [ ^ self ].
	super makeMinimallyResolved.
	
	aliases := self aliases collect: #makeMinimallyResolved.

]

{ #category : #resolving }
RG2TraitAliasDefinition >> makeResolved [

	super makeResolved.
	
	aliases := self aliases.

]

{ #category : #'queries - methods' }
RG2TraitAliasDefinition >> methods [

	"TODO"
	^ self subject methods reject: [ :each | self aliases includes: each name  ]
]

{ #category : #'private - backend access' }
RG2TraitAliasDefinition >> pvtAddAlias: aSymbol [

	aliases isRing2Resolved ifFalse: [ 
		self pvtCleanAliases  ].
	
	aliases add: aSymbol.	
]

{ #category : #'private - backend access' }
RG2TraitAliasDefinition >> pvtAliasesDo: aBlock [

	aliases value do: aBlock
]

{ #category : #'private - backend access' }
RG2TraitAliasDefinition >> pvtCleanAliases [

	aliases := self defaultAliases.
]

{ #category : #'private - backend access' }
RG2TraitAliasDefinition >> pvtRemoveAlias: aSymbol [

	aliases remove: aSymbol.
]

{ #category : #'private - backend access' }
RG2TraitAliasDefinition >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#aliases -> aliases. 
	}
	
	
]

{ #category : #'accessing - backend' }
RG2TraitAliasDefinition >> removeAlias: aSymbol [

	self parent parent announceDefinitionChangeDuring: [ 
		self backend forBehavior removeAlias: aSymbol from: self ].
]

{ #category : #'as yet unclassified' }
RG2TraitAliasDefinition >> traitCompositionString [

	^ String streamContents: [:aStream |	
		aStream 
			nextPutAll: self subject traitTransformationString;
			space;
			nextPut: $@;
			space;
			nextPut: ${.
	self aliases do: [:each | aStream print: each]
		separatedBy: [aStream nextPutAll: '. '].
	aStream nextPut: $}].
]

{ #category : #'as yet unclassified' }
RG2TraitAliasDefinition >> traitTransformationString [

	^ String streamContents: [:aStream |	
		aStream 
			nextPut: $(;
			nextPutAll: self subject name;
			space;
			nextPut: $@;
			space;
			nextPut: ${.
	self aliases do: [:each | aStream print: each]
		separatedBy: [aStream nextPutAll: '. '].
	aStream nextPut: $}.
	aStream nextPut: $)].
]
