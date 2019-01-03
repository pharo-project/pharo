"
I am an internal object used by the ShiftClassBuilder to represent the layout of a class.
"
Class {
	#name : #ShLayoutDefinition,
	#superclass : #Object,
	#instVars : [
		'layoutClass',
		'slots',
		'classSlots',
		'sharedPools',
		'sharedVariables',
		'builder'
	],
	#category : #'Shift-ClassBuilder'
}

{ #category : #accessing }
ShLayoutDefinition >> allSlots [
	| superclass | 
	superclass := builder superclass.
	^ (superclass ifNil: [ #() ] ifNotNil: [ superclass allSlots ]) , slots.
]

{ #category : #accessing }
ShLayoutDefinition >> builder: anObject [
	builder := anObject
]

{ #category : #accessing }
ShLayoutDefinition >> classSlots [
	^ classSlots ifNil: [ #() ]
]

{ #category : #accessing }
ShLayoutDefinition >> classSlots: anObject [
	classSlots := anObject collect: [ :e | e asSlot ].
]

{ #category : #accessing }
ShLayoutDefinition >> copy: slotCollection ifUsedIn: aClass [
	"I only copy the slot if it is not virtual and it is the same in the oldClass. 
	 They are copied as they have the index as instance variable, the index can change in the new class."
	
	aClass slots isEmpty ifTrue: [ ^ slotCollection ].
	
	^ slotCollection
		collect: [ :aSlot | 
			aClass
				slotNamed: aSlot name
				ifFound: [ :oldSlot | 
					(oldSlot == aSlot and: [ aSlot isVirtual not ])
						ifTrue: [ aSlot copy ]
						ifFalse: [ aSlot ] ]
				ifNone: [ aSlot ] ]
]

{ #category : #copying }
ShLayoutDefinition >> copyClassSlotsIfNeeded: oldSlots [ 

	classSlots ifNotNil: [ ^ self ].
	classSlots := (oldSlots select: #isLocal thenCollect: [ :e | e copy index: nil; yourself ])
]

{ #category : #accessing }
ShLayoutDefinition >> copySlotsIfUsedIn: oldClass [
	"I copy the slots if they are used in oldClass. As the slots has state (usually the index) they cannot be shared between the old and the new class."

	oldClass ifNil: [ ^ self ].
	
	slots := self copy: slots ifUsedIn: oldClass.
	classSlots := self copy: self classSlots ifUsedIn: oldClass class.

]

{ #category : #initialization }
ShLayoutDefinition >> initialize [
	super initialize.

	slots := #().
	sharedPools := #().
	sharedVariables := #().

]

{ #category : #testing }
ShLayoutDefinition >> isBits [
	^ self layoutClass new isBits.
]

{ #category : #accessing }
ShLayoutDefinition >> layoutClass [
	^ layoutClass ifNil: [ builder classNamed: #FixedLayout ]
]

{ #category : #accessing }
ShLayoutDefinition >> layoutClass: anObject [
	layoutClass := anObject
]

{ #category : #accessing }
ShLayoutDefinition >> sharedPools [
	^ sharedPools
]

{ #category : #accessing }
ShLayoutDefinition >> sharedPools: anObject [
	sharedPools := anObject
]

{ #category : #printing }
ShLayoutDefinition >> sharedPoolsString [
	^ self sharedPools joinUsing: Character space.
]

{ #category : #accessing }
ShLayoutDefinition >> sharedVariables [
	^ sharedVariables
]

{ #category : #accessing }
ShLayoutDefinition >> sharedVariables: anObject [
	sharedVariables := anObject
]

{ #category : #accessing }
ShLayoutDefinition >> sharedVariablesString [
	^ (sharedVariables collect:[:e | e key]) joinUsing: ' '.
]

{ #category : #accessing }
ShLayoutDefinition >> slots [
	^ slots
]

{ #category : #accessing }
ShLayoutDefinition >> slots: anObject [
	slots := anObject collect: [ :e | e asSlot ].
]
