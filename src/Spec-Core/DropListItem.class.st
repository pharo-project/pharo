"
A DropListItem is an item (wrapper) designed to fit into a DropList
"
Class {
	#name : #DropListItem,
	#superclass : #Object,
	#instVars : [
		'actionHolder',
		'model',
		'displayBlock',
		'icon'
	],
	#category : #'Spec-Core-Widgets-Support'
}

{ #category : #'instance creation' }
DropListItem class >> named: label do: aBlock [

	^ self new
		action: aBlock;
		model: label;
		display: [ :e | e ];
		yourself
]

{ #category : #'instance creation' }
DropListItem class >> on: anObject do: aBlock [

	^ self new
		action: aBlock;
		model: anObject;
		yourself
]

{ #category : #comparing }
DropListItem >> = another [

	self species = another species ifFalse: [ ^ false ].
	^ self label = another label and: [ self actionHolder = another actionHolder ]
]

{ #category : #protocol }
DropListItem >> action: aBlock [

	actionHolder value: aBlock
]

{ #category : #accessing }
DropListItem >> actionHolder [
	
	^ actionHolder
]

{ #category : #protocol }
DropListItem >> display: aBlock [
	"aBlock - 1 optional argument, which is the underlying item; returns the string to be displayed"

	displayBlock := aBlock.
]

{ #category : #accessing }
DropListItem >> icon [
	^ icon
]

{ #category : #accessing }
DropListItem >> icon: anObject [
	icon := anObject
]

{ #category : #initialization }
DropListItem >> initialize [
	super initialize.

	actionHolder := [  ] asValueHolder.
	displayBlock := [ :e | e printString ]
]

{ #category : #accessing }
DropListItem >> label [
	
	^ displayBlock cull: model.
]

{ #category : #accessing }
DropListItem >> label: anObject [
	"For compatibility with old raw-string usage. Send #display: instead"
	
	displayBlock := [ anObject ].
]

{ #category : #accessing }
DropListItem >> model [

	^ model.
]

{ #category : #private }
DropListItem >> model: anObject [

	model := anObject.
]

{ #category : #execution }
DropListItem >> value [
	"This way, I am polymorphic with nil"

	actionHolder value cull: self label cull: self
]
