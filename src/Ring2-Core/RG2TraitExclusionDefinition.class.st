Class {
	#name : #RG2TraitExclusionDefinition,
	#superclass : #RG2TraitTransformationDefinition,
	#instVars : [
		'exclusions'
	],
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'accessing - backend' }
RG2TraitExclusionDefinition >> addExclusion: aSymbol [ 

	self parent parent announceDefinitionChangeDuring: [ 
		self backend forBehavior addExclusion: aSymbol to: self. ].
]

{ #category : #'accessing - backend' }
RG2TraitExclusionDefinition >> cleanExclusions [

	self parent parent announceDefinitionChangeDuring: [ 
		self backend forBehavior cleanExclusionsFor: self. ].
	
]

{ #category : #'default model values' }
RG2TraitExclusionDefinition >> defaultExclusions [

	^ OrderedCollection new
]

{ #category : #'queries - exclusions' }
RG2TraitExclusionDefinition >> exclusions [

	| allExclusions |
	allExclusions := OrderedCollection new.
	self exclusionsDo: [ :each | allExclusions add: each].
	^ allExclusions

]

{ #category : #'accessing - backend' }
RG2TraitExclusionDefinition >> exclusionsDo: aBlock [

	self backend forBehavior exclusionsFor: self do: aBlock
]

{ #category : #initialization }
RG2TraitExclusionDefinition >> initialize [

	super initialize.
	
	exclusions := self unresolvedValue: self defaultExclusions.
]

{ #category : #initialization }
RG2TraitExclusionDefinition >> initializeUnresolved [

	super initializeUnresolved.
	
	exclusions := self unresolvedValue: self defaultExclusions.
]

{ #category : #'testing types' }
RG2TraitExclusionDefinition >> isTraitExclusion [

	^ true
]

{ #category : #resolving }
RG2TraitExclusionDefinition >> makeMinimallyResolved [

	self isRing2Resolved ifTrue: [ ^ self ].
	super makeMinimallyResolved.
	
	exclusions := self exclusions collect: #makeMinimallyResolved.

]

{ #category : #resolving }
RG2TraitExclusionDefinition >> makeResolved [

	super makeResolved.
	
	self resolveExclusions.

]

{ #category : #'queries - methods' }
RG2TraitExclusionDefinition >> methods [

	^ self subject methods reject: [ :each | self exclusions includes: each name  ]
]

{ #category : #'private - backend access' }
RG2TraitExclusionDefinition >> pvtAddExclusion: aSymbol [


	exclusions isRing2Resolved ifFalse: [ 
		self pvtCleanExclusions  ].
	
	exclusions add: aSymbol.	
]

{ #category : #'private - backend access' }
RG2TraitExclusionDefinition >> pvtCleanExclusions [

	exclusions := self defaultExclusions.
]

{ #category : #'private - backend access' }
RG2TraitExclusionDefinition >> pvtExclusionsDo: aBlock [

	exclusions value do: aBlock
]

{ #category : #'private - backend access' }
RG2TraitExclusionDefinition >> pvtRemoveExclusion: aSymbol [

	exclusions remove: aSymbol.
]

{ #category : #'private - backend access' }
RG2TraitExclusionDefinition >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#exclusions -> exclusions. 
	}
	
	
]

{ #category : #'accessing - backend' }
RG2TraitExclusionDefinition >> removeExclusion: aSymbol [

	self parent parent announceDefinitionChangeDuring: [ 
		self backend forBehavior removeExclusion: aSymbol from: self. ].
]

{ #category : #resolving }
RG2TraitExclusionDefinition >> resolveBehaviorInformation [

	self isBehaviorInformationResolved ifTrue: [ ^ self ].
	super resolveBehaviorInformation.
	
	self resolveExclusions.
]

{ #category : #resolving }
RG2TraitExclusionDefinition >> resolveExclusions [
	
	exclusions := self exclusions.

]

{ #category : #traits }
RG2TraitExclusionDefinition >> traitCompositionString [

	^ String streamContents: [:aStream |	
		aStream 
			nextPutAll: self subject name traitTransformationString;
			space;
			nextPut: $-;
			space;
			nextPut: ${.
	self exclusions do: [:each | aStream print: each]
		separatedBy: [aStream nextPutAll: '. '].
	aStream nextPut: $}].
]

{ #category : #'as yet unclassified' }
RG2TraitExclusionDefinition >> traitTransformationString [

	^ String streamContents: [:aStream |	
		aStream 
			nextPut: $(;
			nextPutAll: self subject name;
			space;
			nextPut: $-;
			space;
			nextPut: ${.
	self exclusions do: [:each | aStream print: each]
		separatedBy: [aStream nextPutAll: '. '].
	aStream nextPut: $}.
	aStream nextPut: $).].
]
