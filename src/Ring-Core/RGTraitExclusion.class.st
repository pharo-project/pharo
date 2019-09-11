Class {
	#name : #RGTraitExclusion,
	#superclass : #RGTraitTransformation,
	#instVars : [
		'exclusions'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #'accessing - backend' }
RGTraitExclusion >> addExclusion: aSymbol [ 

	self parent parent announceDefinitionChangeDuring: [ 
		self backend forBehavior addExclusion: aSymbol to: self. ].
]

{ #category : #'accessing - backend' }
RGTraitExclusion >> cleanExclusions [

	self parent parent announceDefinitionChangeDuring: [ 
		self backend forBehavior cleanExclusionsFor: self. ].
	
]

{ #category : #'default model values' }
RGTraitExclusion >> defaultExclusions [

	^ OrderedCollection new
]

{ #category : #'queries - exclusions' }
RGTraitExclusion >> exclusions [

	| allExclusions |
	allExclusions := OrderedCollection new.
	self exclusionsDo: [ :each | allExclusions add: each].
	^ allExclusions

]

{ #category : #'accessing - backend' }
RGTraitExclusion >> exclusionsDo: aBlock [

	self backend forBehavior exclusionsFor: self do: aBlock
]

{ #category : #initialization }
RGTraitExclusion >> initialize [

	super initialize.
	
	exclusions := self unresolvedValue: self defaultExclusions.
]

{ #category : #initialization }
RGTraitExclusion >> initializeUnresolved [

	super initializeUnresolved.
	
	exclusions := self unresolvedValue: self defaultExclusions.
]

{ #category : #'testing types' }
RGTraitExclusion >> isTraitExclusion [

	^ true
]

{ #category : #resolving }
RGTraitExclusion >> makeResolved [

	super makeResolved.
	
	exclusions := self exclusions markAsRingResolved.

]

{ #category : #'queries - methods' }
RGTraitExclusion >> methods [

	^ self subject methods reject: [ :each | self exclusions includes: each name  ]
]

{ #category : #'private - backend access' }
RGTraitExclusion >> pvtAddExclusion: aSymbol [


	exclusions isRingResolved ifFalse: [ 
		self pvtCleanExclusions  ].
	
	exclusions add: aSymbol.	
]

{ #category : #'private - backend access' }
RGTraitExclusion >> pvtCleanExclusions [

	exclusions := self defaultExclusions.
]

{ #category : #'private - backend access' }
RGTraitExclusion >> pvtExclusionsDo: aBlock [

	exclusions value do: aBlock
]

{ #category : #'private - backend access' }
RGTraitExclusion >> pvtRemoveExclusion: aSymbol [

	exclusions remove: aSymbol.
]

{ #category : #'private - backend access' }
RGTraitExclusion >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#exclusions -> exclusions. 
	}
	
	
]

{ #category : #'accessing - backend' }
RGTraitExclusion >> removeExclusion: aSymbol [

	self parent parent announceDefinitionChangeDuring: [ 
		self backend forBehavior removeExclusion: aSymbol from: self. ].
]

{ #category : #traits }
RGTraitExclusion >> traitCompositionString [

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
RGTraitExclusion >> traitTransformationString [

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
