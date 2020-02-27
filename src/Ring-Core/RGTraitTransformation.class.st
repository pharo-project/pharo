Class {
	#name : #RGTraitTransformation,
	#superclass : #RGObject,
	#instVars : [
		'subject'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #'managing container' }
RGTraitTransformation >> addoptToParentStub [

	super addoptToParentStub.
	self parent addTransformation: self.
]

{ #category : #'managing container' }
RGTraitTransformation >> defaultParentStub [

	^ self defaultTraitCompositionStub
]

{ #category : #'default model values' }
RGTraitTransformation >> defaultSubject [

	^ RGUnresolvedValue recursive
]

{ #category : #initialize }
RGTraitTransformation >> initialize [

	super initialize.
	
	subject := self unresolvedValue: self defaultSubject.
]

{ #category : #initialize }
RGTraitTransformation >> initializeUnresolved [

	super initializeUnresolved.
	
	subject := self unresolvedValue: self defaultSubject.
]

{ #category : #'testing types' }
RGTraitTransformation >> isTraitTransformation [

	^ true
]

{ #category : #resolving }
RGTraitTransformation >> makeResolved [

	super makeResolved.
	
	subject := self subject markAsRingResolved.

]

{ #category : #'queries - methods' }
RGTraitTransformation >> methods [

	self subclassResponsibility 
]

{ #category : #'accessing - backend' }
RGTraitTransformation >> name: aString [

	self parent parent announceDefinitionChangeDuring: [ 
		super name: aString ].
]

{ #category : #'private - backend interface' }
RGTraitTransformation >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#subject -> subject. 
	}
	
	
]

{ #category : #'private - backend interface' }
RGTraitTransformation >> pvtSubject [

	^ subject value
]

{ #category : #'private - backend interface' }
RGTraitTransformation >> pvtSubject: anRGTrait [

	self environment verifyOwnership: anRGTrait.

	^ subject := anRGTrait
]

{ #category : #'accessing - backend' }
RGTraitTransformation >> subject [

	^ self backend forBehavior subjectFor: self
]

{ #category : #'accessing - backend' }
RGTraitTransformation >> subject: anRGTrait [

	self parent parent announceDefinitionChangeDuring: [ 
		self backend forBehavior setSubjectFor: self to: anRGTrait. ].
]

{ #category : #'as yet unclassified' }
RGTraitTransformation >> trait [
	
	^ self subject trait
]

{ #category : #'accessing - backend' }
RGTraitTransformation >> usedTraits [

	^ IdentitySet with: subject.
]
