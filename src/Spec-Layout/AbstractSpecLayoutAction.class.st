"
AbstractSpecLayoutAction is an abstract class representing a spec layout action.
"
Class {
	#name : #AbstractSpecLayoutAction,
	#superclass : #Object,
	#instVars : [
		'arguments',
		'selector'
	],
	#category : #'Spec-Layout-Actions'
}

{ #category : #accessing }
AbstractSpecLayoutAction >> arguments [
	^ arguments
]

{ #category : #accessing }
AbstractSpecLayoutAction >> arguments: anObject [
	arguments := anObject
]

{ #category : #protocol }
AbstractSpecLayoutAction >> asSpecElements [

	self subclassResponsibility 
]

{ #category : #protocol }
AbstractSpecLayoutAction >> generateArguments [

	
]

{ #category : #accessing }
AbstractSpecLayoutAction >> selector [
	^ selector
]

{ #category : #accessing }
AbstractSpecLayoutAction >> selector: anObject [
	selector := anObject
]
