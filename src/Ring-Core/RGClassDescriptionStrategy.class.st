Class {
	#name : 'RGClassDescriptionStrategy',
	#superclass : 'RGBehaviorStrategy',
	#instVars : [
		'layout'
	],
	#category : 'Ring-Core-Kernel',
	#package : 'Ring-Core',
	#tag : 'Kernel'
}

{ #category : 'utilities' }
RGClassDescriptionStrategy >> allSlots [

	| superclassSlots aSuperclass |
	aSuperclass := self owner superclass.
	superclassSlots := OrderedCollection new.
	((aSuperclass == self owner) or: [aSuperclass isNil ]) ifFalse: [
		 superclassSlots addAll: aSuperclass allSlots ].
	^ (superclassSlots, self layout allSlots) asArray
]

{ #category : 'initialization' }
RGClassDescriptionStrategy >> classVariablesString [
	"Answer a string of my class variable names separated by spaces."

	^ String streamContents: [ :stream |
		self owner classVarNames
			do: [ :each | stream nextPutAll: each ]
			separatedBy: [ stream space ] ]
]

{ #category : 'copying' }
RGClassDescriptionStrategy >> copyForBehaviorDefinitionPostCopy [

	super copyForBehaviorDefinitionPostCopy.
	layout := self layout copyForBehaviorDefinition.
	layout parent: self owner
]

{ #category : 'initialization' }
RGClassDescriptionStrategy >> defaultLayout [

	^ self owner defaultFixedLayoutStubIn: self
]

{ #category : 'initialization' }
RGClassDescriptionStrategy >> initialize [

	super initialize.

	layout := self unresolvedValue: self defaultLayout.
	self owner environment backend createUnresolvedClassGroupFor: self owner
]

{ #category : 'initialization' }
RGClassDescriptionStrategy >> initializeUnresolved [

	super initializeUnresolved.

	layout := self unresolvedValue: self defaultLayout
]

{ #category : 'initialization' }
RGClassDescriptionStrategy >> instSize [

	^ self owner allInstVarNames size
]

{ #category : 'initialization' }
RGClassDescriptionStrategy >> instVarNames [
	^ self layout instVarNames
]

{ #category : 'accessing' }
RGClassDescriptionStrategy >> instanceVariables: aCollectionOfSymbols [

	"specify instance variable names"

	(self owner hasUnresolved: #layout)
		ifTrue: [ self layout: (RGFixedLayout parent: self)].

	(self layout isPointerLayout not and: [ aCollectionOfSymbols isEmpty ])
		ifTrue: [ ^ self ].

	self layout cleanSlots.

	aCollectionOfSymbols do: [ :instVarName |
		layout addSlot: (RGInstanceVariableSlot named: instVarName asSymbol parent: self)]
]

{ #category : 'accessing' }
RGClassDescriptionStrategy >> instanceVariablesString [
	"Answer a string of my instance variable names separated by spaces."

	^String streamContents: [ :stream |
		self instVarNames
			do: [ :each | stream nextPutAll: each ]
			separatedBy: [ stream space ] ]
]

{ #category : 'private - backend interface' }
RGClassDescriptionStrategy >> makeResolved [

	super makeResolved.

	layout := self layout makeResolved markAsRingResolved
]

{ #category : 'private - backend interface' }
RGClassDescriptionStrategy >> pvtLayout [

	^ layout value
]

{ #category : 'private - backend interface' }
RGClassDescriptionStrategy >> pvtLayout: anRGLayout [

	self environment verifyOwnership: anRGLayout.

	^ layout := anRGLayout
]

{ #category : 'private - backend interface' }
RGClassDescriptionStrategy >> pvtResolvableProperties [

	^ super pvtResolvableProperties, {
		#layout -> layout.
     	}
]

{ #category : 'storing' }
RGClassDescriptionStrategy >> storeOn: aStream [
	aStream
		nextPutAll: '(';
		nextPutAll: self storeName;
		nextPutAll: ' named: '.
	self owner  name storeOn: aStream.
	aStream nextPut: $)
]
