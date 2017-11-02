"
I am used to get the correct menu of common text areas
"
Class {
	#name : #RubPluggableGetMenuPolicy,
	#superclass : #RubGetMenuPolicy,
	#instVars : [
		'getMenuSelector',
		'menuProvider'
	],
	#category : 'Rubric-Editing-Core'
}

{ #category : #accessing }
RubPluggableGetMenuPolicy >> getMenuSelector [
	^ getMenuSelector
]

{ #category : #accessing }
RubPluggableGetMenuPolicy >> getMenuSelector: aSymbol [
	getMenuSelector := aSymbol
]

{ #category : #'accessing menu' }
RubPluggableGetMenuPolicy >> lookupMenu [
	(getMenuSelector isNil or: [ menuProvider isNil ])
		ifTrue: [ ^ super lookupMenu ].
	^ (WeakMessageSend receiver: menuProvider selector: getMenuSelector) cull: MenuMorph new cull: false
]

{ #category : #'accessing menu' }
RubPluggableGetMenuPolicy >> lookupShiftMenu [
	(getMenuSelector isNil or: [ menuProvider isNil ])
		ifTrue: [ ^ super lookupMenu ].
	^ (WeakMessageSend receiver: menuProvider selector: getMenuSelector) cull: MenuMorph new cull: true
]

{ #category : #accessing }
RubPluggableGetMenuPolicy >> menuProvider [
	^ menuProvider
]

{ #category : #accessing }
RubPluggableGetMenuPolicy >> menuProvider: anObject [
	menuProvider := anObject
]
