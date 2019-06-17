"
I am example class which deprecated methods
"
Class {
	#name : #ClyClassWithDeprecatedMethods,
	#superclass : #Object,
	#category : #'Calypso-SystemPlugins-Deprecation-Queries-Tests'
}

{ #category : #'as yet unclassified' }
ClyClassWithDeprecatedMethods >> deprecatedMethod1 [
	self deprecated: 'it is deprecated method for testing deprecation analyzis'
]

{ #category : #'as yet unclassified' }
ClyClassWithDeprecatedMethods >> deprecatedMethod2 [
	self deprecated: 'it is deprecated method for testing deprecation analyzis'
]

{ #category : #'as yet unclassified' }
ClyClassWithDeprecatedMethods >> notDeprecatedMethod [

]
