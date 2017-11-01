"
I am the superclass for all layouts with Slots.
"
Class {
	#name : #PointerLayout,
	#superclass : #ObjectLayout,
	#instVars : [
		'slotScope'
	],
	#category : 'Slot-Layout'
}

{ #category : #comparing }
PointerLayout >> = other [
	^ super = other
		ifFalse: [  false ]
		ifTrue: [ self slotScope = other slotScope ]
]

{ #category : #accessing }
PointerLayout >> allSlots [
	^ slotScope flatten
]

{ #category : #accessing }
PointerLayout >> allVisibleSlots [
	^ slotScope allVisibleSlots
]

{ #category : #validation }
PointerLayout >> checkInheritedSlots [
	
	self host superclass 
		ifNil: [ ^ self ].
	self host classLayout
		ifNil: [ ^ self ].
		
	self 
		assert: (self host superclass classLayout size <= self host classLayout size)
		description: [ (self host name, ' does not inherit all slots from ', self host superclass name) ].
	
	"check that the we have the same slots at the same indices as in the superclass"
	self host superclass classLayout allSlots withIndexDo: [ :slot :index|
		| localSlot |
		localSlot := (self host classLayout allSlots at: index).
		self 
			assert: slot = localSlot
			description: [ 'Slot "', localSlot name, '" index at position ', index asString, ' conflicts with slot "', slot name, '" in ', self host superclass asString ]].
]

{ #category : #validation }
PointerLayout >> checkIntegrity [
	self
		checkSanity;
		checkParentScopes;
		checkInheritedSlots
]

{ #category : #validation }
PointerLayout >> checkParentScopes [
	| parentScope superclassScope |
	parentScope := self slotScope parentScope.
	self host superclass ifNil: [ ^ self ].
	superclassScope := self host superclass classLayout slotScope.
	self 
		assert: parentScope = superclassScope
		description: 'Parent slot scope is out of sync'
]

{ #category : #validation }
PointerLayout >> checkSanity [
	super checkSanity.
	self 
		checkSlotNames;
		checkSlotIndices
]

{ #category : #validation }
PointerLayout >> checkSlotIndices [
	| slots current |
	slots := slotScope flatten select: [:each | each size > 0]. "skip slots that have no index"
	[ slots size > 1 ] whileTrue: [
		current := slots removeFirst.
			self assert: slots first index = (current index + current size) ]
]

{ #category : #validation }
PointerLayout >> checkSlotNames [
	| slots current |
	slots := slotScope allVisibleSlots.
	[ slots isEmpty ] whileFalse: [
		current := slots removeLast.
		slots do: [ :other |
			other name = current name
				ifTrue: [ 
					DuplicatedSlotName new
						oldSlot: current;
						newSlot: other;
						host: host;
						signal ]]]
]

{ #category : #diff }
PointerLayout >> computeChangesFrom: other in: modification [
	| additions changes removals copies |
	other hasSlots ifFalse: [ ^ modification additions: self allSlots asArray ].
	
	additions := self allSlots.
	removals := OrderedCollection new.
	copies := modification copies.
	changes := modification changes.
	
	other allSlots do: [ :oldSlot |
		(self popSlot: oldSlot from: additions)
			ifNil: [ removals add: oldSlot ]
			ifNotNil: [ :newSlot |
				newSlot = oldSlot
					ifTrue: [ copies at: newSlot put: oldSlot ]
					ifFalse: [ changes at: newSlot put: oldSlot ]]].
			
	modification
		additions: additions asArray;
		removals: removals asArray.
]

{ #category : #api }
PointerLayout >> definesSlotNamed: aString [ 
	^self resolveSlot: aString ifFound: [ true ] ifNone: [ false ].
]

{ #category : #extending }
PointerLayout >> extend [
	"Answer a default layout extending me."

	^ self extend: self slotScope extend
]

{ #category : #extending }
PointerLayout >> extend: aScope [
	^ self species new slotScope: aScope
]

{ #category : #reshaping }
PointerLayout >> extendAgain: aLayout with: someScope [
	| result |
	result := aLayout copy.
	result slotScope: someScope.
	^ result
]

{ #category : #extending }
PointerLayout >> extendEphemeron: newScope [

	^ EphemeronLayout new slotScope: newScope
]

{ #category : #extending }
PointerLayout >> extendVariable: newScope [
	^ VariableLayout new slotScope: newScope
]

{ #category : #extending }
PointerLayout >> extendWeak: newScope [
	^ WeakLayout new slotScope: newScope
]

{ #category : #accessing }
PointerLayout >> fieldSize [
	^ slotScope fieldSize
]

{ #category : #testing }
PointerLayout >> hasFields [
	^ slotScope hasFields
]

{ #category : #testing }
PointerLayout >> hasSlots [
	^ slotScope hasSlots
]

{ #category : #comparing }
PointerLayout >> hash [
	^ self class hash bitXor: self slotScope hash
]

{ #category : #'instance initialization' }
PointerLayout >> initializeInstance: anInstance [
	self allSlotsDo: [ :slot | slot initialize: anInstance ]
]

{ #category : #accessing }
PointerLayout >> instVarIndexFor: aString ifAbsent: aBlockClosure [
	| idx |
	idx := 1.
	slotScope do: [:each |
		each isVisible ifTrue: [
			each name = aString ifTrue: [^idx].
			idx := idx +1]].
	^aBlockClosure value
]

{ #category : #accessing }
PointerLayout >> instVarNames [
	^ slotScope visibleSlotNames
]

{ #category : #diff }
PointerLayout >> popSlot: aSlot from: collection [
	collection withIndexDo: [ :slot :index |
		slot name == aSlot name
			ifTrue: [ ^ collection removeAt: index ]].
	^ nil
]

{ #category : #copying }
PointerLayout >> postCopy [
	slotScope := slotScope copy
]

{ #category : #reshaping }
PointerLayout >> reshapeTo: aModification [
	| newScope |
	newScope := slotScope rebase: aModification originalScope to: aModification newScope.
	^ aModification newLayout extendAgain: self with: newScope.
]

{ #category : #accessing }
PointerLayout >> resolveSlot: aName [
	^ slotScope atName: aName
]

{ #category : #accessing }
PointerLayout >> resolveSlot: aName ifFound: foundBlock ifNone: noneBlock [
	^ slotScope atName: aName ifFound: foundBlock ifNone: noneBlock
]

{ #category : #testing }
PointerLayout >> size [ 
	^ slotScope fieldSize
]

{ #category : #api }
PointerLayout >> slotNamed: aName [ 
	^self resolveSlot: aName asSymbol
]

{ #category : #accessing }
PointerLayout >> slotScope [
	^ slotScope
]

{ #category : #accessing }
PointerLayout >> slotScope: anObject [
	slotScope := anObject
]

{ #category : #accessing }
PointerLayout >> slots [
	^ slotScope slots
]
