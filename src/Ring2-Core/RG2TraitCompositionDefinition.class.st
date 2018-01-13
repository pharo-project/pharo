Class {
	#name : #RG2TraitCompositionDefinition,
	#superclass : #RG2Definition,
	#instVars : [
		'transformations'
	],
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'accessing - backend' }
RG2TraitCompositionDefinition >> addTransformation: aRG2TraitTransformationDefinition [ 

	self parent announceDefinitionChangeDuring: [ 
		self backend forBehavior addTransformation: aRG2TraitTransformationDefinition to: self ].
]

{ #category : #'managing container' }
RG2TraitCompositionDefinition >> addoptToParentStub [

	super addoptToParentStub.

	self environment backend createUnresolvedClassGroupFor: self parent
]

{ #category : #'accessing - backend' }
RG2TraitCompositionDefinition >> cleanTransformations [

	self parent announceDefinitionChangeDuring: [ 
		self backend forBehavior cleanTransformationsFor: self ].
]

{ #category : #'managing container' }
RG2TraitCompositionDefinition >> defaultParentStub [

	^ self defaultClassStub
]

{ #category : #'as yet unclassified' }
RG2TraitCompositionDefinition >> defaultTransformations [

	^ OrderedCollection new.
]

{ #category : #initialization }
RG2TraitCompositionDefinition >> initialize [

	super initialize.
	
	transformations := self unresolvedValue: self defaultTransformations.
]

{ #category : #initialization }
RG2TraitCompositionDefinition >> initializeUnresolved [

	super initializeUnresolved.
	
	transformations := self unresolvedValue: self defaultTransformations.
]

{ #category : #'testing types' }
RG2TraitCompositionDefinition >> isTraitComposition [

	^ true
]

{ #category : #resolving }
RG2TraitCompositionDefinition >> makeMinimallyResolved [

	self isRing2Resolved ifTrue: [ ^ self ].
	super makeMinimallyResolved.
	
	transformations := self transformations collect: #makeMinimallyResolved.

]

{ #category : #resolving }
RG2TraitCompositionDefinition >> makeResolved [

	super makeResolved.
	
	self resolveTransformations.

]

{ #category : #'queries - methods' }
RG2TraitCompositionDefinition >> methods [

	^ Array streamContents: [ :stream | 
		self transformationsDo: [ :transformation | stream nextPutAll: transformation methods ]]
]

{ #category : #'private - backend access' }
RG2TraitCompositionDefinition >> pvtAddTransformation: aRG2TraitTransformationDefinition [

	self environment verifyOwnership: aRG2TraitTransformationDefinition.
	
	transformations isRing2Resolved ifFalse: [ 
		self pvtCleanTransformations  ].
	
	transformations add: aRG2TraitTransformationDefinition.	
]

{ #category : #'private - backend access' }
RG2TraitCompositionDefinition >> pvtCleanTransformations [

	transformations := self defaultTransformations.
]

{ #category : #'private - backend access' }
RG2TraitCompositionDefinition >> pvtRemoveTransformation: aRG2TraitTransformationDefinition [

	self environment verifyOwnership: aRG2TraitTransformationDefinition.
	
	transformations remove: aRG2TraitTransformationDefinition.
]

{ #category : #'private - backend access' }
RG2TraitCompositionDefinition >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#transformations -> transformations. 
	}
	
	
]

{ #category : #'private - backend access' }
RG2TraitCompositionDefinition >> pvtTransformationsDo: aBlock [

	transformations value do: aBlock
]

{ #category : #'as yet unclassified' }
RG2TraitCompositionDefinition >> referencedBehaviors [

	^ self transformations collect: [ :each |
		each isTrait 
			ifTrue: [ each ]
			ifFalse: [ each subject ] ]
]

{ #category : #'accessing - backend' }
RG2TraitCompositionDefinition >> removeTransformation: aRG2TraitTransformationDefinition [

	self parent announceDefinitionChangeDuring: [ 
		self backend forBehavior removeTransformation: aRG2TraitTransformationDefinition from: self ].
]

{ #category : #resolving }
RG2TraitCompositionDefinition >> resolveBehaviorInformation [

	self isBehaviorInformationResolved ifTrue: [ ^ self ].
	super resolveBehaviorInformation.
	
	self resolveTransformations.
	self transformationsDo: [ :each | each resolveBehaviorInformation ].
]

{ #category : #resolving }
RG2TraitCompositionDefinition >> resolveTransformations [

	transformations := self transformations.
]

{ #category : #'as yet unclassified' }
RG2TraitCompositionDefinition >> traitCompositionString [

	self transformations isEmpty ifTrue: [ ^ '{}' ].
	
	^String streamContents: [ :stream | 
		self transformations 
			do: [ :each | stream nextPutAll: each traitTransformationString ]
			separatedBy: [ stream nextPutAll: ' + ' ] ]	
	
]

{ #category : #'as yet unclassified' }
RG2TraitCompositionDefinition >> traits [

	^ self transformations collect: [:each | each trait]
]

{ #category : #'queries - transformations' }
RG2TraitCompositionDefinition >> transformations [

	| allTransformations |
	allTransformations := OrderedCollection new.
	self transformationsDo: [ :each | allTransformations add: each].
	^ allTransformations

]

{ #category : #'accessing - backend' }
RG2TraitCompositionDefinition >> transformationsDo: aBlock [

	self backend forBehavior transformationsFor: self do: aBlock
]

{ #category : #'as yet unclassified' }
RG2TraitCompositionDefinition >> usedTraits [

	^ self transformations flatCollect: [ :each | 
		each isTrait 
			ifTrue: [ {each} ]
			ifFalse: [ each usedTraits ].
		 ] as: IdentitySet.
]
