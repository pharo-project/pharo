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

{ #category : #accesing }
AbstractSpecLayoutAction >> arguments [
	^ arguments
]

{ #category : #accesing }
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

{ #category : #accesing }
AbstractSpecLayoutAction >> selector [
	^ selector
]

{ #category : #accesing }
AbstractSpecLayoutAction >> selector: anObject [
	selector := anObject
]
