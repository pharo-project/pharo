"
I am example of class with abstract methods
"
Class {
	#name : #ClyAbstractClassMock,
	#superclass : #Object,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #protocol }
ClyAbstractClassMock >> abstractMethod1 [
	self subclassResponsibility 
]

{ #category : #protocol }
ClyAbstractClassMock >> abstractMethod2 [
	self subclassResponsibility 
]

{ #category : #protocol }
ClyAbstractClassMock >> overriddenMethod [
	^#originalValue
]
