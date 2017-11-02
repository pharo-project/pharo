"
I am the last layout scope in a scope chain.
"
Class {
	#name : #LayoutEmptyScope,
	#superclass : #LayoutAbstractScope,
	#classInstVars : [
		'instance'
	],
	#category : 'Slot-Layout'
}

{ #category : #accessing }
LayoutEmptyScope class >> instance [
	^ instance ifNil: [ instance := self new ]
]

{ #category : #enumerating }
LayoutEmptyScope >> allSlotsDo: aBlock [
]

{ #category : #enumerating }
LayoutEmptyScope >> allSlotsReverseDo: aBlock [
]

{ #category : #accessing }
LayoutEmptyScope >> allVisibleSlots [
	^ OrderedCollection new
]

{ #category : #accessing }
LayoutEmptyScope >> fieldSize [
	^ 0
]

{ #category : #flattening }
LayoutEmptyScope >> flatten [
	^ OrderedCollection new
]

{ #category : #testing }
LayoutEmptyScope >> hasFields [
	^ false
]

{ #category : #testing }
LayoutEmptyScope >> hasSlots [
	^ false
]

{ #category : #testing }
LayoutEmptyScope >> ifNotEmpty: aBlock [
	"This scope is empty so we do nothing"
]

{ #category : #accessing }
LayoutEmptyScope >> ownFieldSize [
	^ 0
]

{ #category : #accessing }
LayoutEmptyScope >> visibleSlots [
	^ #()
]

{ #category : #enumerating }
LayoutEmptyScope >> withParentScopesDo: aBlock [
	aBlock value: self.
]
