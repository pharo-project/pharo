Class {
	#name : #ClyItemObserverExample,
	#superclass : #Object,
	#instVars : [
		'wasNotified'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #controlling }
ClyItemObserverExample >> itemsChanged [
	wasNotified := true
]

{ #category : #testing }
ClyItemObserverExample >> wasNotified [
	^wasNotified ifNil: [ false ]
]
