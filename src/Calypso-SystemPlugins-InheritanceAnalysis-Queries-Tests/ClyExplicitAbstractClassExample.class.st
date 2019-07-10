"
I am example of a class which is explicitly definined as abstract by overrideing class side method #isAbstract
"
Class {
	#name : #ClyExplicitAbstractClassExample,
	#superclass : #ClyAbstractClassExample,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #testing }
ClyExplicitAbstractClassExample class >> isAbstract [
	^true
]
