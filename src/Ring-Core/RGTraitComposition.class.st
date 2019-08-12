Class {
	#name : #RGTraitComposition,
	#superclass : #RGObject,
	#instVars : [
		'transformations'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #'accessing - backend' }
RGTraitComposition >> addTransformation: anRGTraitTransformation [ 

	self parent announceDefinitionChangeDuring: [ 
		self backend forBehavior addTransformation: anRGTraitTransformation to: self ].
]

{ #category : #'managing container' }
RGTraitComposition >> addoptToParentStub [

	super addoptToParentStub.

	self environment backend createUnresolvedClassGroupFor: self parent
]

{ #category : #'accessing - backend' }
RGTraitComposition >> cleanTransformations [

	self parent announceDefinitionChangeDuring: [ 
		self backend forBehavior cleanTransformationsFor: self ].
]

{ #category : #'managing container' }
RGTraitComposition >> defaultParentStub [

	^ self defaultClassStub
]

{ #category : #'as yet unclassified' }
RGTraitComposition >> defaultTransformations [

	^ OrderedCollection new.
]

{ #category : #initialization }
RGTraitComposition >> initialize [

	super initialize.
	
	transformations := self unresolvedValue: self defaultTransformations.
]

{ #category : #initialization }
RGTraitComposition >> initializeUnresolved [

	super initializeUnresolved.
	
	transformations := self unresolvedValue: self defaultTransformations.
]

{ #category : #'testing types' }
RGTraitComposition >> isTraitComposition [

	^ true
]

{ #category : #resolving }
RGTraitComposition >> makeResolved [

	super makeResolved.
	
	transformations := self transformations markAsRingResolved.

]

{ #category : #'queries - methods' }
RGTraitComposition >> methods [

	^ Array streamContents: [ :stream | 
		self transformationsDo: [ :transformation | stream nextPutAll: transformation methods ]]
]

{ #category : #'private - backend access' }
RGTraitComposition >> pvtAddTransformation: anRGTraitTransformation [

	self environment verifyOwnership: anRGTraitTransformation.
	
	transformations isRingResolved ifFalse: [ 
		self pvtCleanTransformations  ].
	
	transformations add: anRGTraitTransformation.	
]

{ #category : #'private - backend access' }
RGTraitComposition >> pvtCleanTransformations [

	transformations := self defaultTransformations.
]

{ #category : #'private - backend access' }
RGTraitComposition >> pvtRemoveTransformation: anRGTraitTransformation [

	self environment verifyOwnership: anRGTraitTransformation.
	
	transformations remove: anRGTraitTransformation.
]

{ #category : #'private - backend access' }
RGTraitComposition >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#transformations -> transformations. 
	}
	
	
]

{ #category : #'private - backend access' }
RGTraitComposition >> pvtTransformationsDo: aBlock [

	transformations value do: aBlock
]

{ #category : #'as yet unclassified' }
RGTraitComposition >> referencedBehaviors [

	^ self transformations collect: [ :each |
		each isTrait 
			ifTrue: [ each ]
			ifFalse: [ each subject ] ]
]

{ #category : #'accessing - backend' }
RGTraitComposition >> removeTransformation: anRGTraitTransformation [

	self parent announceDefinitionChangeDuring: [ 
		self backend forBehavior removeTransformation: anRGTraitTransformation from: self ].
]

{ #category : #'as yet unclassified' }
RGTraitComposition >> traitCompositionString [

	self transformations isEmpty ifTrue: [ ^ '{}' ].
	
	^String streamContents: [ :stream | 
		self transformations 
			do: [ :each | stream nextPutAll: each traitTransformationString ]
			separatedBy: [ stream nextPutAll: ' + ' ] ]	
	
]

{ #category : #'as yet unclassified' }
RGTraitComposition >> traits [

	^ self transformations collect: [:each | each trait]
]

{ #category : #'queries - transformations' }
RGTraitComposition >> transformations [

	| allTransformations |
	allTransformations := OrderedCollection new.
	self transformationsDo: [ :each | allTransformations add: each].
	^ allTransformations

]

{ #category : #'accessing - backend' }
RGTraitComposition >> transformationsDo: aBlock [

	self backend forBehavior transformationsFor: self do: aBlock
]

{ #category : #'as yet unclassified' }
RGTraitComposition >> usedTraits [

	^ self transformations flatCollect: [ :each | 
		each isTrait 
			ifTrue: [ {each} ]
			ifFalse: [ each usedTraits ].
		 ] as: IdentitySet.
]
