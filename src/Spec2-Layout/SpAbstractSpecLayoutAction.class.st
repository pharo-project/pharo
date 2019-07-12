"
AbstractSpecLayoutAction is an abstract class representing a spec layout action.
"
Class {
	#name : #SpAbstractSpecLayoutAction,
	#superclass : #Object,
	#instVars : [
		'selector'
	],
	#category : #'Spec2-Layout-Deprecated'
}

{ #category : #deprecation }
SpAbstractSpecLayoutAction class >> isDeprecated [
	"Layouts from Spec 1 will be removed in Pharo 9 because the implementation is not good. Please use the new SpecExecutableLayout subclasses instead"

	^ true
]

{ #category : #accesing }
SpAbstractSpecLayoutAction >> arguments [
	^ self subclassResponsibility
]

{ #category : #protocol }
SpAbstractSpecLayoutAction >> asSpecElements [
	^ {self selector} , self arguments
]

{ #category : #accesing }
SpAbstractSpecLayoutAction >> selector [
	^ selector
]

{ #category : #accesing }
SpAbstractSpecLayoutAction >> selector: anObject [
	selector := anObject
]
