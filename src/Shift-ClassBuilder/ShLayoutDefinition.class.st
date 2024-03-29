"
I am an internal object used by the ShiftClassBuilder to represent the layout of a class.
"
Class {
	#name : 'ShLayoutDefinition',
	#superclass : 'Object',
	#instVars : [
		'layoutClass',
		'slots',
		'classSlots',
		'sharedPools',
		'sharedVariables',
		'builder'
	],
	#category : 'Shift-ClassBuilder-Builder',
	#package : 'Shift-ClassBuilder',
	#tag : 'Builder'
}

{ #category : 'accessing' }
ShLayoutDefinition >> allSlots [
	| superclass |
	superclass := builder superclass.
	^ (superclass ifNil: [ #() ] ifNotNil: [ superclass allSlots ]) , slots
]

{ #category : 'accessing' }
ShLayoutDefinition >> builder: anObject [
	builder := anObject
]

{ #category : 'accessing' }
ShLayoutDefinition >> classSlots [

	^ classSlots
]

{ #category : 'accessing' }
ShLayoutDefinition >> classSlots: anObject [
	classSlots := anObject collect: [ :e | e asSlot ]
]

{ #category : 'initialization' }
ShLayoutDefinition >> initialize [

	super initialize.

	slots := #(  ).
	classSlots := #(  ).
	sharedPools := #(  ).
	sharedVariables := #(  )
]

{ #category : 'testing' }
ShLayoutDefinition >> isBits [
	^ self layoutClass new isBits
]

{ #category : 'accessing' }
ShLayoutDefinition >> layoutClass [
	^ layoutClass ifNil: [ builder classNamed: #FixedLayout ]
]

{ #category : 'accessing' }
ShLayoutDefinition >> layoutClass: anObject [
	layoutClass := anObject
]

{ #category : 'accessing' }
ShLayoutDefinition >> sharedPools [
	^ sharedPools
]

{ #category : 'accessing' }
ShLayoutDefinition >> sharedPools: anObject [
	sharedPools := anObject
]

{ #category : 'printing' }
ShLayoutDefinition >> sharedPoolsString [
	^ self sharedPools joinUsing: Character space
]

{ #category : 'accessing' }
ShLayoutDefinition >> sharedVariables [
	^ sharedVariables
]

{ #category : 'accessing' }
ShLayoutDefinition >> sharedVariables: anObject [
	sharedVariables := anObject
]

{ #category : 'accessing' }
ShLayoutDefinition >> slots [
	^ slots
]

{ #category : 'accessing' }
ShLayoutDefinition >> slots: anObject [
	slots := anObject collect: [ :e | e asSlot ]
]

{ #category : 'validating' }
ShLayoutDefinition >> validate [

	| slotNames classSlotNames |
	builder oldClass ifNil: [ ^ self ].

	slotNames := slots collect: [ :e | e name ].
	classSlotNames := classSlots collect: [ :e | e name ].

	builder oldClass subclassesDo: [ :aSubclass |
		aSubclass slots do: [ :anotherSlot |
			(slotNames includes: anotherSlot name) ifTrue: [
				DuplicatedSlotName new
						oldSlot: anotherSlot;
						newSlot: nil;
						host: aSubclass;
						signal
			] ]	 ].

	builder oldClass subclassesDo: [ :aSubclass |
		aSubclass class slots do: [ :anotherSlot |
			(classSlotNames includes: anotherSlot name) ifTrue: [
				DuplicatedSlotName new
						oldSlot: anotherSlot;
						newSlot: nil;
						host: aSubclass class;
						signal
			] ]	 ]
]
