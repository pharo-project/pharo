Class {
	#name : #SpecMockMenu,
	#superclass : #Object,
	#instVars : [
		'realMenu',
		'shownHolder'
	],
	#category : #'Spec-BackendTests'
}

{ #category : #initialization }
SpecMockMenu >> initialize [

	super initialize.
	shownHolder := false asValueHolder
]

{ #category : #accessing }
SpecMockMenu >> realMenu [
	^ realMenu
]

{ #category : #accessing }
SpecMockMenu >> realMenu: anObject [
	realMenu := anObject
]

{ #category : #accessing }
SpecMockMenu >> shown [

	^ shownHolder value
]

{ #category : #accessing }
SpecMockMenu >> shown: anObject [

	shownHolder value: anObject
]

{ #category : #accessing }
SpecMockMenu >> whenShownDo: aBlock [

	shownHolder whenChangedDo: aBlock
]
