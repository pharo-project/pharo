"
A DropListItem is an item (wrapper) designed to fit into a DropList
"
Class {
	#name : #SpDropListItem,
	#superclass : #Object,
	#instVars : [
		'actionHolder',
		'model',
		'displayBlock',
		'icon'
	],
	#category : #'Spec2-Core-Widgets-Support'
}

{ #category : #'instance creation' }
SpDropListItem class >> named: label do: aBlock [

	^ self new
		action: aBlock;
		model: label;
		display: [ :e | e ];
		yourself
]

{ #category : #'instance creation' }
SpDropListItem class >> on: anObject do: aBlock [

	^ self new
		action: aBlock;
		model: anObject;
		yourself
]

{ #category : #comparing }
SpDropListItem >> = another [

	self species = another species ifFalse: [ ^ false ].
	^ self label = another label and: [ self actionHolder = another actionHolder ]
]

{ #category : #protocol }
SpDropListItem >> action: aBlock [

	actionHolder value: aBlock
]

{ #category : #accessing }
SpDropListItem >> actionHolder [
	
	^ actionHolder
]

{ #category : #protocol }
SpDropListItem >> display: aBlock [
	"aBlock - 1 optional argument, which is the underlying item; returns the string to be displayed"

	displayBlock := aBlock.
]

{ #category : #accessing }
SpDropListItem >> icon [
	^ icon
]

{ #category : #accessing }
SpDropListItem >> icon: anObject [
	icon := anObject
]

{ #category : #initialization }
SpDropListItem >> initialize [

	super initialize.

	actionHolder := [] asValueHolder.
	displayBlock := [ :e | e asString ].
	icon := nil
]

{ #category : #accessing }
SpDropListItem >> label [
	
	^ displayBlock cull: model.
]

{ #category : #accessing }
SpDropListItem >> label: anObject [
	"For compatibility with old raw-string usage. Send #display: instead"
	
	displayBlock := [ anObject ].
]

{ #category : #accessing }
SpDropListItem >> model [

	^ model.
]

{ #category : #private }
SpDropListItem >> model: anObject [

	model := anObject.
]

{ #category : #execution }
SpDropListItem >> value [
	"This way, I am polymorphic with nil"

	actionHolder value cull: self label cull: self
]
