"
I am example of class which inherits from abstract superclass
"
Class {
	#name : #ClyClassWhichInheritsAbstractClassMock,
	#superclass : #ClyAbstractClassMock,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #protocol }
ClyClassWhichInheritsAbstractClassMock >> abstractMethod1 [
	^#result
]

{ #category : #protocol }
ClyClassWhichInheritsAbstractClassMock >> notOverriddenMethod [

]

{ #category : #protocol }
ClyClassWhichInheritsAbstractClassMock >> overriddenMethod [
	^#newValue
]
