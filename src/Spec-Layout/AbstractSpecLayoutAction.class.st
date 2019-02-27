"
AbstractSpecLayoutAction is an abstract class representing a spec layout action.
"
Class {
	#name : #AbstractSpecLayoutAction,
	#superclass : #Object,
	#instVars : [
		'selector'
	],
	#category : #'Spec-Layout-Actions'
}

{ #category : #accesing }
AbstractSpecLayoutAction >> arguments [
	^ self subclassResponsibility
]

{ #category : #protocol }
AbstractSpecLayoutAction >> asSpecElements [
	^ {self selector} , self arguments
]

{ #category : #accesing }
AbstractSpecLayoutAction >> selector [
	^ selector
]

{ #category : #accesing }
AbstractSpecLayoutAction >> selector: anObject [
	selector := anObject
]
