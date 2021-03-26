Class {
	#name : #MockClyFullBrowserClassContext,
	#superclass : #Object,
	#instVars : [
		'classDefinitionEditor',
		'editor'
	],
	#category : #'Calypso-SystemTools-Core-Tests-Editors-Classes'
}

{ #category : #helpers }
MockClyFullBrowserClassContext class >> banana [
	^ 1
]

{ #category : #accessing }
MockClyFullBrowserClassContext >> lastSelectedClass [
	^ { self class }
]

{ #category : #accessing }
MockClyFullBrowserClassContext >> selectedClassSide [ 
	^ self class class
]
