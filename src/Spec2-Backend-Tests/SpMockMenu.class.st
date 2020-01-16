Class {
	#name : #SpMockMenu,
	#superclass : #Object,
	#traits : 'SpTObservable',
	#classTraits : 'SpTObservable classTrait',
	#instVars : [
		'#realMenu',
		'#shown => SpObservableSlot'
	],
	#category : #'Spec2-Backend-Tests'
}

{ #category : #initialization }
SpMockMenu >> initialize [
	
	self class initializeSlots: self.
	super initialize.
	shown := false
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
	^ shown
]

{ #category : #accessing }
SpMockMenu >> shown: anObject [
	shown := anObject
]

{ #category : #accessing }
SpMockMenu >> whenShownDo: aBlock [
	self property: #shown whenChangedDo: aBlock
]
