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
	#category : 'Shift-ClassBuilder'
}

{ #category : #accessing }
ShLayoutDefinition >> builder: anObject [
	builder := anObject
]

{ #category : #accessing }
ShLayoutDefinition >> classSlots [
	^ classSlots
]

{ #category : #accessing }
ShLayoutDefinition >> classSlots: anObject [
	classSlots := anObject collect: [ :e | e asSlot ].
]

{ #category : #accessing }
ShLayoutDefinition >> copySlotsIfUsedIn: oldClass [
	oldClass ifNil:[^self].
	
	oldClass slots isEmpty ifTrue: [ ^self ].
	oldClass classLayout postCopy.		

	oldClass class slots isEmpty ifTrue: [ ^self ].
	oldClass class classLayout postCopy.		

]

{ #category : #initialization }
ShLayoutDefinition >> initialize [
	super initialize.

	slots := #().
	classSlots := #().
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
