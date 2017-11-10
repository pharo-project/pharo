"
I am a small wrapper used to encapsulate data created and used during spec interpretation
"
Class {
	#name : #SpecWrapper,
	#superclass : #Object,
	#instVars : [
		'instance',
		'selector'
	],
	#category : #Spec-Core
}

{ #category : #'instance creation' }
SpecWrapper class >> instance: instance selector: selector [

	^ self new
		instance: instance;
		selector: selector;
		yourself
]

{ #category : #accessing }
SpecWrapper >> instance [
	
	^ instance
]

{ #category : #accessing }
SpecWrapper >> instance: anObject [
	
	instance := anObject
]

{ #category : #protocol }
SpecWrapper >> isRedrawable [
	
	^ self instance isRedrawable
]

{ #category : #protocol }
SpecWrapper >> removeSubWidgets [
	
	^ self instance removeSubWidgets
]

{ #category : #accessing }
SpecWrapper >> selector [
	
	^ selector
]

{ #category : #accessing }
SpecWrapper >> selector: anObject [
	
	selector := anObject
]
