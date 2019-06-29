Class {
	#name : #SpMockMenu,
	#superclass : #Object,
	#instVars : [
		'realMenu',
		'shownHolder'
	],
	#category : #'Spec2-Backend-Tests'
}

{ #category : #initialization }
SpMockMenu >> initialize [

	super initialize.
	shownHolder := false asValueHolder
]

{ #category : #accessing }
SpMockMenu >> realMenu [
	^ realMenu
]

{ #category : #accessing }
SpMockMenu >> realMenu: anObject [
	realMenu := anObject
]

{ #category : #accessing }
SpMockMenu >> shown [

	^ shownHolder value
]

{ #category : #accessing }
SpMockMenu >> shown: anObject [

	shownHolder value: anObject
]

{ #category : #accessing }
SpMockMenu >> whenShownDo: aBlock [

	shownHolder whenChangedDo: aBlock
]
