"
I am example of class which inherits from abstract superclass
"
Class {
	#name : #ClyClassWhichInheritsAbstractClass,
	#superclass : #ClyAbstractClassExample,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #'as yet unclassified' }
ClyClassWhichInheritsAbstractClass >> abstractMethod1 [
	^#result
]

{ #category : #'as yet unclassified' }
ClyClassWhichInheritsAbstractClass >> notOverriddenMethod [

]

{ #category : #'as yet unclassified' }
ClyClassWhichInheritsAbstractClass >> overriddenMethod [
	^#newValue
]
