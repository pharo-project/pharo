"
A DropListItem is an item (wrapper) designed to fit into a DropList
"
Class {
	#name : #SpDropListItem,
	#superclass : #Object,
	#instVars : [
		'model',
		'displayBlock',
		'icon',
		'action'
	],
	#category : #'Spec2-Core-Support'
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
SpDropListItem >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ self label = anObject label and: [ action = anObject action ]
]

{ #category : #protocol }
SpDropListItem >> action [
	^ action
]

{ #category : #protocol }
SpDropListItem >> action: aBlock [
	action := aBlock
]

{ #category : #protocol }
SpDropListItem >> display: aBlock [
	"aBlock - 1 optional argument, which is the underlying item; returns the string to be displayed"

	displayBlock := aBlock.
]

{ #category : #comparing }
SpDropListItem >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ self label hash bitXor: action hash
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
	action := [  ].
	displayBlock := [ :e | e asString ]
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

	self action cull: self label cull: self
]
