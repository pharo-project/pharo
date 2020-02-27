Class {
	#name : #RGTraitAlias,
	#superclass : #RGTraitTransformation,
	#instVars : [
		'aliases'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #'accessing - backend' }
RGTraitAlias >> addAlias: aSymbol [ 

	self parent parent announceDefinitionChangeDuring: [ 
		self backend forBehavior addAlias: aSymbol to: self ].
]

{ #category : #'queries - aliases' }
RGTraitAlias >> aliases [

	| allAliases |
	allAliases := OrderedCollection new.
	self aliasesDo: [ :each | allAliases add: each].
	^ allAliases 

]

{ #category : #'accessing - backend' }
RGTraitAlias >> aliasesDo: aBlock [

	self backend forBehavior aliasesFor: self do: aBlock
]

{ #category : #'accessing - backend' }
RGTraitAlias >> cleanAliases [

	self parent parent announceDefinitionChangeDuring: [ 
		self backend forBehavior cleanAliasesFor: self ].
]

{ #category : #'default model values' }
RGTraitAlias >> defaultAliases [

	^ OrderedCollection new
]

{ #category : #initialize }
RGTraitAlias >> initialize [

	super initialize.
	
	aliases := self unresolvedValue: self defaultAliases.
]

{ #category : #initialize }
RGTraitAlias >> initializeUnresolved [

	super initializeUnresolved.
	
	aliases := self unresolvedValue: self defaultAliases.
]

{ #category : #'testing types' }
RGTraitAlias >> isTraitAlias [

	^ true
]

{ #category : #resolving }
RGTraitAlias >> makeResolved [

	super makeResolved.
	
	aliases := self aliases markAsRingResolved.

]

{ #category : #'queries - methods' }
RGTraitAlias >> methods [

	"TODO"
	^ self subject methods reject: [ :each | self aliases includes: each name  ]
]

{ #category : #'private - backend access' }
RGTraitAlias >> pvtAddAlias: aSymbol [

	aliases isRingResolved ifFalse: [ 
		self pvtCleanAliases  ].
	
	aliases add: aSymbol.	
]

{ #category : #'private - backend access' }
RGTraitAlias >> pvtAliasesDo: aBlock [

	aliases value do: aBlock
]

{ #category : #'private - backend access' }
RGTraitAlias >> pvtCleanAliases [

	aliases := self defaultAliases.
]

{ #category : #'private - backend access' }
RGTraitAlias >> pvtRemoveAlias: aSymbol [

	aliases remove: aSymbol.
]

{ #category : #'private - backend access' }
RGTraitAlias >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#aliases -> aliases. 
	}
	
	
]

{ #category : #'accessing - backend' }
RGTraitAlias >> removeAlias: aSymbol [

	self parent parent announceDefinitionChangeDuring: [ 
		self backend forBehavior removeAlias: aSymbol from: self ].
]

{ #category : #'as yet unclassified' }
RGTraitAlias >> traitCompositionString [

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
RGTraitAlias >> traitTransformationString [

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
