"
I am example of class with abstract methods
"
Class {
	#name : #ClyAbstractClassExample,
	#superclass : #Object,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #'as yet unclassified' }
ClyAbstractClassExample >> abstractMethod1 [
	self subclassResponsibility 
]

{ #category : #'as yet unclassified' }
ClyAbstractClassExample >> abstractMethod2 [
	self subclassResponsibility 
]

{ #category : #'as yet unclassified' }
ClyAbstractClassExample >> overriddenMethod [
	^#originalValue
]
