Class {
	#name : #RG2TraitTransformationDefinition,
	#superclass : #RG2Definition,
	#instVars : [
		'subject'
	],
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'managing container' }
RG2TraitTransformationDefinition >> addoptToParentStub [

	super addoptToParentStub.
	self parent addTransformation: self.
]

{ #category : #'managing container' }
RG2TraitTransformationDefinition >> defaultParentStub [

	^ self defaultTraitCompositionStub
]

{ #category : #'default model values' }
RG2TraitTransformationDefinition >> defaultSubject [

	^ RG2UnresolvedValue recursive
]

{ #category : #initialize }
RG2TraitTransformationDefinition >> initialize [

	super initialize.
	
	subject := self unresolvedValue: self defaultSubject.
]

{ #category : #initialize }
RG2TraitTransformationDefinition >> initializeUnresolved [

	super initializeUnresolved.
	
	subject := self unresolvedValue: self defaultSubject.
]

{ #category : #'testing types' }
RG2TraitTransformationDefinition >> isTraitTransformation [

	^ true
]

{ #category : #resolving }
RG2TraitTransformationDefinition >> makeMinimallyResolved [

	self isRing2Resolved ifTrue: [ ^ self ].
	super makeMinimallyResolved.
	
	subject := self subject makeMinimallyResolved.

]

{ #category : #resolving }
RG2TraitTransformationDefinition >> makeResolved [

	super makeResolved.
	
	self resolveSubject.

]

{ #category : #'queries - methods' }
RG2TraitTransformationDefinition >> methods [

	self subclassResponsibility 
]

{ #category : #'accessing - backend' }
RG2TraitTransformationDefinition >> name: aString [

	self parent parent announceDefinitionChangeDuring: [ 
		super name: aString ].
]

{ #category : #'private - backend interface' }
RG2TraitTransformationDefinition >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#subject -> subject. 
	}
	
	
]

{ #category : #'private - backend interface' }
RG2TraitTransformationDefinition >> pvtSubject [

	^ subject value
]

{ #category : #'private - backend interface' }
RG2TraitTransformationDefinition >> pvtSubject: aRG2TraitDefinition [

	self environment verifyOwnership: aRG2TraitDefinition.

	^ subject := aRG2TraitDefinition
]

{ #category : #resolving }
RG2TraitTransformationDefinition >> resolveBehaviorInformation [

	self isBehaviorInformationResolved ifTrue: [ ^ self ].
	super resolveBehaviorInformation.
	
	self resolveSubject.
	self subject resolveName.

]

{ #category : #resolving }
RG2TraitTransformationDefinition >> resolveSubject [

	subject := self subject.
]

{ #category : #'accessing - backend' }
RG2TraitTransformationDefinition >> subject [

	^ self backend forBehavior subjectFor: self
]

{ #category : #'accessing - backend' }
RG2TraitTransformationDefinition >> subject: aRG2TraitDefinition [

	self parent parent announceDefinitionChangeDuring: [ 
		self backend forBehavior setSubjectFor: self to: aRG2TraitDefinition. ].
]

{ #category : #'as yet unclassified' }
RG2TraitTransformationDefinition >> trait [
	
	^ self subject trait
]

{ #category : #'accessing - backend' }
RG2TraitTransformationDefinition >> usedTraits [

	^ IdentitySet with: subject.
]
