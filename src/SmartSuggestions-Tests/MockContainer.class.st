"
I'm only a fake container, since in some cases to build a context we use the wraped model, and build a full container isn't so simple... 
"
Class {
	#name : #MockContainer,
	#superclass : #Object,
	#instVars : [
		'model'
	],
	#category : #SmartSuggestions-Tests-Mocks
}

{ #category : #'instance creation' }
MockContainer class >> for: model [
	^ self new 
		model: model; 
		yourself.
]

{ #category : #accessing }
MockContainer >> model [
	^ model.
]

{ #category : #accessing }
MockContainer >> model: anObject [
	model := anObject
]
