Class {
	#name : #RGClassDescriptionStrategy,
	#superclass : #RGBehaviorStrategy,
	#instVars : [
		'layout'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #utilities }
RGClassDescriptionStrategy >> allSlots [

	| superclassSlots aSuperclass |
	aSuperclass := self owner superclass.
	superclassSlots := OrderedCollection new.
	((aSuperclass == self owner) or: [aSuperclass isNil ]) ifFalse: [ 
		 superclassSlots addAll: aSuperclass allSlots ].
	^ (superclassSlots, self layout allSlots) asArray.
]

{ #category : #initialization }
RGClassDescriptionStrategy >> classVariablesString [
	"Answer a string of my class variable names separated by spaces."

	^ String streamContents: [ :stream | 
		self owner classVarNames 
			do: [ :each | stream nextPutAll: each ]
			separatedBy: [ stream space ] ]
]

{ #category : #copying }
RGClassDescriptionStrategy >> copyForBehaviorDefinitionPostCopy [

	super copyForBehaviorDefinitionPostCopy.
	layout := self layout copyForBehaviorDefinition.
	layout parent: self owner.

]

{ #category : #initialization }
RGClassDescriptionStrategy >> defaultLayout [

	^ self owner defaultFixedLayoutStubIn: self.
]

{ #category : #initialization }
RGClassDescriptionStrategy >> initialize [

	super initialize.
	
	layout := self unresolvedValue: self defaultLayout. 
	self owner environment backend createUnresolvedClassGroupFor: self owner.
]

{ #category : #initialization }
RGClassDescriptionStrategy >> initializeUnresolved [

	super initializeUnresolved.
	
	layout := self unresolvedValue: self defaultLayout.

]

{ #category : #initialization }
RGClassDescriptionStrategy >> instSize [

	^ self owner allInstVarNames size
]

{ #category : #initialization }
RGClassDescriptionStrategy >> instVarNames [
	^ self layout instVarNames
]

{ #category : #accessing }
RGClassDescriptionStrategy >> instanceVariables: aCollectionOfSymbols [

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
RGClassDescriptionStrategy >> instanceVariablesString [
	"Answer a string of my instance variable names separated by spaces."

	^String streamContents: [ :stream |
		self instVarNames 
			do: [ :each | stream nextPutAll: each ]
			separatedBy: [ stream space ] ]
]

{ #category : #'testing class hierarchy' }
RGClassDescriptionStrategy >> kindOfSubclass [
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
RGClassDescriptionStrategy >> layout [

	^ self backend forBehavior layoutFor: self owner
]

{ #category : #'accessing - backend' }
RGClassDescriptionStrategy >> layout: anRGLayout [

	self owner announceDefinitionChangeDuring: [ 
		self backend forBehavior setLayoutFor: self owner to: anRGLayout ].

]

{ #category : #'private - backend interface' }
RGClassDescriptionStrategy >> makeResolved [

	super makeResolved. 
	
	layout := self layout makeResolved markAsRingResolved.

]

{ #category : #'private - backend interface' }
RGClassDescriptionStrategy >> pvtLayout [

	^ layout value
]

{ #category : #'private - backend interface' }
RGClassDescriptionStrategy >> pvtLayout: anRGLayout [

	self environment verifyOwnership: anRGLayout.

	^ layout := anRGLayout
]

{ #category : #'private - backend interface' }
RGClassDescriptionStrategy >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#layout -> layout. 
     	}
]

{ #category : #printing }
RGClassDescriptionStrategy >> storeOn: aStream [
	aStream
		nextPutAll: '(';
		nextPutAll: self storeName;
		nextPutAll: ' named: '.
	self owner  name storeOn: aStream.
	aStream nextPut: $)
]
