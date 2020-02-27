Class {
	#name : #RGPointerLayout,
	#superclass : #RGLayout,
	#instVars : [
		'slots'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #'accessing - backend' }
RGPointerLayout >> addSlot: anRGSlot [ 

	self parent announceDefinitionChangeDuring: [ 
		self backend forBehavior addSlot: anRGSlot to: self ].
]

{ #category : #'queries - slots' }
RGPointerLayout >> allSlots [ 

	^ self slots
]

{ #category : #'accessing - backend' }
RGPointerLayout >> cleanSlots [

	self parent announceDefinitionChangeDuring: [ 
		self backend forBehavior cleanSlotsFor: self ].
	
	
]

{ #category : #'default model values' }
RGPointerLayout >> defaultSlots [

	^ OrderedCollection new
]

{ #category : #initialization }
RGPointerLayout >> initialize [

	super initialize.
	slots := self unresolvedValue: self defaultSlots.


]

{ #category : #initialization }
RGPointerLayout >> initializeUnresolved [

	super initializeUnresolved.
	slots := self unresolvedValue: self defaultSlots.


]

{ #category : #'queries - slots' }
RGPointerLayout >> instVarNames [
	^ self slots collect: #name
]

{ #category : #'testing types' }
RGPointerLayout >> isPointerLayout [

	^ true
]

{ #category : #accessing }
RGPointerLayout >> layoutName [

	^ #PointerLayout
]

{ #category : #resolving }
RGPointerLayout >> makeResolved [

	super makeResolved.
	
	slots := self slots asOrderedCollection markAsRingResolved.

]

{ #category : #'private - backend access' }
RGPointerLayout >> pvtAddSlot: anRGSlot [

	self environment verifyOwnership: anRGSlot.
	
	slots isRingResolved ifFalse: [ 
		self pvtCleanSlots  ].
	
	slots add: anRGSlot.	
]

{ #category : #'private - backend access' }
RGPointerLayout >> pvtCleanSlots [

	slots := self defaultSlots.
]

{ #category : #'private - backend access' }
RGPointerLayout >> pvtRemoveSlot: anRGSlot [

	self environment verifyOwnership: anRGSlot.
	
	slots remove: anRGSlot.
	

]

{ #category : #'private - backend access' }
RGPointerLayout >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#slots -> slots. 
	}
	
	
]

{ #category : #'private - backend access' }
RGPointerLayout >> pvtSlotsDo: aBlock [

	slots value do: aBlock
]

{ #category : #'accessing - backend' }
RGPointerLayout >> removeSlot: anRGSlot [

	self parent announceDefinitionChangeDuring: [ 
		self backend forBehavior removeSlot: anRGSlot from: self ].
]

{ #category : #'queries - slots' }
RGPointerLayout >> slots [

	| allSlots |
	allSlots := OrderedCollection new.
	self slotsDo: [ :each | allSlots add: each].
	^ allSlots asArray
]

{ #category : #'accessing - backend' }
RGPointerLayout >> slotsDo: aBlock [

	self backend forBehavior slotsFor: self do: aBlock
]
