Class {
	#name : #RGTraitV2DescriptionStrategy,
	#superclass : #RGBehaviorStrategy,
	#instVars : [
		'layout'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #utilities }
RGTraitV2DescriptionStrategy >> allSlots [

	| superclassSlots aSuperclass |
	aSuperclass := self owner superclass.
	superclassSlots := OrderedCollection new.
	((aSuperclass == self owner) or: [aSuperclass isNil ]) ifFalse: [ 
		 superclassSlots addAll: aSuperclass allSlots ].
	^ (superclassSlots, self layout allSlots) asArray.
]

{ #category : #initialization }
RGTraitV2DescriptionStrategy >> classVariablesString [
	"Answer a string of my class variable names separated by spaces."

	^ String streamContents: [ :stream | 
		self owner classVarNames 
			do: [ :each | stream nextPutAll: each ]
			separatedBy: [ stream space ] ]
]

{ #category : #copying }
RGTraitV2DescriptionStrategy >> copyForBehaviorDefinitionPostCopy [

	super copyForBehaviorDefinitionPostCopy.
	layout := self layout copyForBehaviorDefinition.
	layout parent: self owner.

]

{ #category : #initialization }
RGTraitV2DescriptionStrategy >> defaultLayout [

	^ self owner defaultFixedLayoutStubIn: self.
]

{ #category : #initialization }
RGTraitV2DescriptionStrategy >> initialize [

	super initialize.
	
	layout := self unresolvedValue: self defaultLayout. 
	self owner environment backend createUnresolvedClassGroupFor: self owner.
]

{ #category : #initialization }
RGTraitV2DescriptionStrategy >> initializeUnresolved [

	super initializeUnresolved.
	
	layout := self unresolvedValue: self defaultLayout.

]

{ #category : #initialization }
RGTraitV2DescriptionStrategy >> instSize [

	^ self owner allInstVarNames size
]

{ #category : #initialization }
RGTraitV2DescriptionStrategy >> instVarNames [
	^ self layout instVarNames
]

{ #category : #accessing }
RGTraitV2DescriptionStrategy >> instanceVariables: aCollectionOfSymbols [

	"specify instance variable names"

	(self owner hasUnresolved: #layout)
		ifTrue: [ self layout: (RGFixedLayout parent: self)].
		
	(self layout isPointerLayout not and: [ aCollectionOfSymbols isEmpty ])
		ifTrue: [ ^ self ].
		
	self layout cleanSlots.

	aCollectionOfSymbols do: [ :instVarName |
		layout addSlot: (RGInstanceVariableSlot named: instVarName asSymbol parent: self)].
]

{ #category : #accessing }
RGTraitV2DescriptionStrategy >> instanceVariablesString [
	"Answer a string of my instance variable names separated by spaces."

	^String streamContents: [ :stream |
		self instVarNames 
			do: [ :each | stream nextPutAll: each ]
			separatedBy: [ stream space ] ]
]

{ #category : #'private - backend interface' }
RGTraitV2DescriptionStrategy >> isTrait [

	^true
]

{ #category : #'testing class hierarchy' }
RGTraitV2DescriptionStrategy >> kindOfSubclass [
	"Answer a String that is the keyword that describes the receiver's kind of subclass,
	 either a regular subclass, a variableSubclass, a variableByteSubclass,
	 a variableWordSubclass, a weakSubclass, an ephemeronSubclass or an immediateSubclass.
	 c.f. typeOfClass"
	^self owner isVariable
		ifTrue:
			[self layout isBitsLayout
				ifTrue:
					[self layout isByteLayout
						ifTrue: [' variableByteSubclass: ']
						ifFalse: [' variableWordSubclass: ']]
				ifFalse:
					[self layout isWeakLayout
						ifTrue: [' weakSubclass: ']
						ifFalse: [' variableSubclass: ']]]
		ifFalse:
			[self layout isImmediateLayout 
				ifTrue: [' immediateSubclass: ']
				ifFalse:
					[self layout isEphemeronLayout 
						ifTrue: [' ephemeronSubclass: ']
						ifFalse: [' subclass: ']]]
]

{ #category : #'accessing - backend' }
RGTraitV2DescriptionStrategy >> layout [

	^ self backend forBehavior layoutFor: self owner
]

{ #category : #'accessing - backend' }
RGTraitV2DescriptionStrategy >> layout: anRGLayout [

	self owner announceDefinitionChangeDuring: [ 
		self backend forBehavior setLayoutFor: self owner to: anRGLayout ].

]

{ #category : #'private - backend interface' }
RGTraitV2DescriptionStrategy >> makeResolved [

	super makeResolved. 
	
	layout := self layout makeResolved markAsRingResolved.

]

{ #category : #'private - backend interface' }
RGTraitV2DescriptionStrategy >> pvtLayout [

	^ layout value
]

{ #category : #'private - backend interface' }
RGTraitV2DescriptionStrategy >> pvtLayout: anRGLayout [

	self owner environment verifyOwnership: anRGLayout.

	^ layout := anRGLayout
]

{ #category : #'private - backend interface' }
RGTraitV2DescriptionStrategy >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#layout -> layout. 
     	}
]

{ #category : #printing }
RGTraitV2DescriptionStrategy >> storeOn: aStream [
	aStream
		nextPutAll: '(';
		nextPutAll: self storeName;
		nextPutAll: ' named: '.
	self owner  name storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #variables }
RGTraitV2DescriptionStrategy >> trait [

	^ self owner
]

{ #category : #'private - backend access' }
RGTraitV2DescriptionStrategy >> traitTransformationString [		
		
	^ self owner name 
]
