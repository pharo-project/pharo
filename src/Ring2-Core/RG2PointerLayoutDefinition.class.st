Class {
	#name : #RG2PointerLayoutDefinition,
	#superclass : #RG2LayoutDefinition,
	#instVars : [
		'slots'
	],
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'accessing - backend' }
RG2PointerLayoutDefinition >> addSlot: aRG2SlotDefinition [ 

	self parent announceDefinitionChangeDuring: [ 
		self backend forBehavior addSlot: aRG2SlotDefinition to: self ].
]

{ #category : #'as yet unclassified' }
RG2PointerLayoutDefinition >> allSlots [ 

	^ self slots
]

{ #category : #'accessing - backend' }
RG2PointerLayoutDefinition >> cleanSlots [

	self parent announceDefinitionChangeDuring: [ 
		self backend forBehavior cleanSlotsFor: self ].
	
	
]

{ #category : #'default model values' }
RG2PointerLayoutDefinition >> defaultSlots [

	^ OrderedCollection new
]

{ #category : #initialization }
RG2PointerLayoutDefinition >> initialize [

	super initialize.
	slots := self unresolvedValue: self defaultSlots.


]

{ #category : #initialization }
RG2PointerLayoutDefinition >> initializeUnresolved [

	super initializeUnresolved.
	slots := self unresolvedValue: self defaultSlots.


]

{ #category : #'queries - slots' }
RG2PointerLayoutDefinition >> instVarNames [
	^ self slots collect: #name
]

{ #category : #'testing types' }
RG2PointerLayoutDefinition >> isPointerLayout [

	^ true
]

{ #category : #resolving }
RG2PointerLayoutDefinition >> makeMinimallyResolved [

	self isRing2Resolved ifTrue: [ ^ self ].
	super makeMinimallyResolved.
	
	slots := self slots asOrderedCollection collect: #makeMinimallyResolved.

]

{ #category : #resolving }
RG2PointerLayoutDefinition >> makeResolved [

	super makeResolved.
	
	slots := self slots asOrderedCollection.

]

{ #category : #'private - backend access' }
RG2PointerLayoutDefinition >> pvtAddSlot: aRG2SlotDefinition [

	self environment verifyOwnership: aRG2SlotDefinition.
	
	slots isRing2Resolved ifFalse: [ 
		self pvtCleanSlots  ].
	
	slots add: aRG2SlotDefinition.	
]

{ #category : #'private - backend access' }
RG2PointerLayoutDefinition >> pvtCleanSlots [

	slots := self defaultSlots.
]

{ #category : #'private - backend access' }
RG2PointerLayoutDefinition >> pvtRemoveSlot: aRG2SlotDefinition [

	self environment verifyOwnership: aRG2SlotDefinition.
	
	slots remove: aRG2SlotDefinition.
	

]

{ #category : #'private - backend access' }
RG2PointerLayoutDefinition >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#slots -> slots. 
	}
	
	
]

{ #category : #'private - backend access' }
RG2PointerLayoutDefinition >> pvtSlotsDo: aBlock [

	slots value do: aBlock
]

{ #category : #'accessing - backend' }
RG2PointerLayoutDefinition >> removeSlot: aRG2SlotDefinition [

	self parent announceDefinitionChangeDuring: [ 
		self backend forBehavior removeSlot: aRG2SlotDefinition from: self ].
]

{ #category : #'queries - slots' }
RG2PointerLayoutDefinition >> slots [

	| allSlots |
	allSlots := OrderedCollection new.
	self slotsDo: [ :each | allSlots add: each].
	^ allSlots asArray
]

{ #category : #'accessing - backend' }
RG2PointerLayoutDefinition >> slotsDo: aBlock [

	self backend forBehavior slotsFor: self do: aBlock
]
