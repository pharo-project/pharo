"
I am example of a class which is explicitly definined as abstract by overrideing class side method #isAbstract
"
Class {
	#name : #ClyExplicitAbstractClassExampleMock,
	#superclass : #ClyAbstractClassMock,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #testing }
ClyExplicitAbstractClassExampleMock class >> isAbstract [
	^true
]
