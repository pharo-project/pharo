"
RG2ClassDescriptionDefinition is the common parent for classes and metaclasses defining instance variables
"
Class {
	#name : #RG2ClassDescriptionDefinition,
	#superclass : #RG2BehaviorDefinition,
	#instVars : [
		'layout'
	],
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'as yet unclassified' }
RG2ClassDescriptionDefinition >> allSlots [

	| superclassSlots |
	superclassSlots := OrderedCollection new.
	((self superclass == self) or: [self superclass isNil ]) ifFalse: [ 
		 superclassSlots addAll: self superclass allSlots ].
	^ (superclassSlots, self layout allSlots) asArray.
]

{ #category : #'as yet unclassified' }
RG2ClassDescriptionDefinition >> classVariablesString [
	"Answer a string of my class variable names separated by spaces."

	^String streamContents: [ :stream | 
		self classVarNames 
			do: [ :each | stream nextPutAll: each ]
			separatedBy: [ stream space ] ]
]

{ #category : #copying }
RG2ClassDescriptionDefinition >> copyForBehaviorDefinitionPostCopy [

	super copyForBehaviorDefinitionPostCopy.
	
	layout := self layout copyForBehaviorDefinition.
	layout parent: self.



]

{ #category : #'default model values' }
RG2ClassDescriptionDefinition >> defaultMetaClass [

	^ self environment backend createNewUnresolvedMetaclassFor: self
]

{ #category : #'as yet unclassified' }
RG2ClassDescriptionDefinition >> findSuitableParentIn: aRG2Environment [

	^ aRG2Environment packages detect: [:package |
		package name = self package name ] 
]

{ #category : #initialization }
RG2ClassDescriptionDefinition >> initialize [

	super initialize.
	
	layout := self unresolvedValue: self defaultLayout. 
	self environment backend createUnresolvedClassGroupFor: self.
	
	

	"metaClass := self defaultMetaClass."

]

{ #category : #initialization }
RG2ClassDescriptionDefinition >> initializeUnresolved [

	super initializeUnresolved. 
	layout := self unresolvedValue: self defaultLayout. 


"	metaClass := self defaultMetaClass.
"
]

{ #category : #'as yet unclassified' }
RG2ClassDescriptionDefinition >> instSize [

	^ self allInstVarNames size
]

{ #category : #'as yet unclassified' }
RG2ClassDescriptionDefinition >> instVarNames [
	^ self layout instVarNames
]

{ #category : #'as yet unclassified' }
RG2ClassDescriptionDefinition >> instanceVariables: aCollectionOfSymbols [

	"specify instance variable names"

	(self hasUnresolved: #layout)
		ifTrue: [ self layout: (RG2FixedLayoutDefinition parent: self)].
		
	(self layout isPointerLayout not and: [ aCollectionOfSymbols isEmpty ])
		ifTrue: [ ^ self ].
		
	self layout cleanSlots.

	aCollectionOfSymbols do: [ :instVarName |
		layout addSlot: (RG2InstanceVariableSlotDefinition named: instVarName asSymbol parent: self)].
]

{ #category : #printing }
RG2ClassDescriptionDefinition >> instanceVariablesString [
	"Answer a string of my instance variable names separated by spaces."

	^String streamContents: [ :stream |
		self instVarNames 
			do: [ :each | stream nextPutAll: each ]
			separatedBy: [ stream space ] ]
]

{ #category : #testing }
RG2ClassDescriptionDefinition >> isBehavior [

	^ true
]

{ #category : #'as yet unclassified' }
RG2ClassDescriptionDefinition >> isBits [

	^ self layout isBitsLayout
]

{ #category : #testing }
RG2ClassDescriptionDefinition >> isBytes [

	^ self layout isByteLayout 
]

{ #category : #testing }
RG2ClassDescriptionDefinition >> isCompiledMethod [

	^ self layout isCompiledMethodLayout 
]

{ #category : #testing }
RG2ClassDescriptionDefinition >> isEphemeron [

	^ self layout isEphemeronLayout
]

{ #category : #testing }
RG2ClassDescriptionDefinition >> isEphemeronClass [

	^ self layout isEphemeronLayout 
]

{ #category : #testing }
RG2ClassDescriptionDefinition >> isImmediateClass [

	^ self layout isImmediateLayout  
]

{ #category : #testing }
RG2ClassDescriptionDefinition >> isPointers [

	^ self isBits not
]

{ #category : #'as yet unclassified' }
RG2ClassDescriptionDefinition >> isVariable [

	"is the definition a variable class?"

	^ self layout isVariableLayout
]

{ #category : #testing }
RG2ClassDescriptionDefinition >> isWeak [

	^ self layout isWeakLayout
]

{ #category : #testing }
RG2ClassDescriptionDefinition >> isWords [

	^ self layout isWordLayout 
]

{ #category : #'testing class hierarchy' }
RG2ClassDescriptionDefinition >> kindOfSubclass [
	"Answer a String that is the keyword that describes the receiver's kind of subclass,
	 either a regular subclass, a variableSubclass, a variableByteSubclass,
	 a variableWordSubclass, a weakSubclass, an ephemeronSubclass or an immediateSubclass.
	 c.f. typeOfClass"
	^self isVariable
		ifTrue:
			[self isBits
				ifTrue:
					[self isBytes
						ifTrue: [' variableByteSubclass: ']
						ifFalse: [' variableWordSubclass: ']]
				ifFalse:
					[self isWeak
						ifTrue: [' weakSubclass: ']
						ifFalse: [' variableSubclass: ']]]
		ifFalse:
			[self isImmediateClass
				ifTrue: [' immediateSubclass: ']
				ifFalse:
					[self isEphemeronClass
						ifTrue: [' ephemeronSubclass: ']
						ifFalse: [' subclass: ']]]
]

{ #category : #'accessing - backend' }
RG2ClassDescriptionDefinition >> layout [

	^ self backend forBehavior layoutFor: self
]

{ #category : #'accessing - backend' }
RG2ClassDescriptionDefinition >> layout: aRG2LayoutDefinition [

	self announceDefinitionChangeDuring: [ 
		self backend forBehavior setLayoutFor: self to: aRG2LayoutDefinition ].
]

{ #category : #resolving }
RG2ClassDescriptionDefinition >> makeResolved [

	super makeResolved.
	
	layout := self layout.

]

{ #category : #'private - backend interface' }
RG2ClassDescriptionDefinition >> pvtLayout [

	^ layout value
]

{ #category : #'private - backend interface' }
RG2ClassDescriptionDefinition >> pvtLayout: aRG2LayoutDefinition [

	self environment verifyOwnership: aRG2LayoutDefinition.

	^ layout := aRG2LayoutDefinition
]

{ #category : #'private - backend interface' }
RG2ClassDescriptionDefinition >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#layout -> layout. 
     	}
]

{ #category : #resolving }
RG2ClassDescriptionDefinition >> setNameFromSiblingIfNeeded [

	self subclassResponsibility 
]

{ #category : #'as yet unclassified' }
RG2ClassDescriptionDefinition >> sharedPools [

	^ OrderedCollection new.
]

{ #category : #'as yet unclassified' }
RG2ClassDescriptionDefinition >> sharedPoolsString [
	"Answer a string of my class variable names separated by spaces."

	"TODO: check validity"
	^String streamContents: [ :stream | 
		self sharedPools 
			do: [ :each | stream nextPutAll: each name]
			separatedBy: [ stream space ] ]
]

{ #category : #printing }
RG2ClassDescriptionDefinition >> slotDefinitionString [
	"Answer a string that contains an executable description of my Slots"

	"^self slots ifNotEmpty: [self slots asString] ifEmpty: ['{}']"
	
	^String streamContents: [ :str | | special |
		str nextPutAll: '{ '.
		self slots do: [:slot |
				str nextPutAll: slot definitionString.
				special := slot isSpecial]				
			separatedBy: [ 
				str nextPutAll: '. '.  
				special ifTrue: [ str cr;tab;tab;tab;tab ]].
		str nextPutAll: ' }'. ]

]

{ #category : #slots }
RG2ClassDescriptionDefinition >> slots [		
		
	^ self layout slots 
]

{ #category : #printing }
RG2ClassDescriptionDefinition >> storeOn: aStream [
	aStream
		nextPutAll: '(';
		nextPutAll: self class name;
		nextPutAll: ' named: '.
	name storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #slots }
RG2ClassDescriptionDefinition >> usesSpecialSlot [
	"return true if we define something else than InstanceVariableSlots"
	^self slots anySatisfy: [ :each | each isSpecial ]
]
