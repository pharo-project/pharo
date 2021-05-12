"
I am example of abstact class which is subclass of another abstract class.
I define new extra abstract methods
"
Class {
	#name : #ClyAbstractSubclassMock,
	#superclass : #ClyAbstractClassMock,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #protocol }
ClyAbstractSubclassMock >> abstractMethod1 [
]

{ #category : #protocol }
ClyAbstractSubclassMock >> abstractSubclassMethod [
	self subclassResponsibility 
]
