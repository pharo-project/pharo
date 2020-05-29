"
I am a base of event definitions for SpEventHandler. 
My children will define (and install in an adaptor/widget) a specific kind of low-level event.

"
Class {
	#name : #SpBaseEventDefinition,
	#superclass : #Object,
	#instVars : [
		'action'
	],
	#category : #'Spec2-Core-Base-Event'
}

{ #category : #enumerating }
SpBaseEventDefinition class >> do: aBlock [

	^ self new action: aBlock
]

{ #category : #accessing }
SpBaseEventDefinition >> action [
	^ action
]

{ #category : #accessing }
SpBaseEventDefinition >> action: anObject [
	action := anObject
]

{ #category : #installing }
SpBaseEventDefinition >> installOn: anAdapter target: aWidget [

	self subclassResponsibility
]
