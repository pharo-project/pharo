"
I return all deprecated methods from my scope
"
Class {
	#name : #ClyDeprecatedMethods,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-Deprecation-Queries'
}

{ #category : #printing }
ClyDeprecatedMethods >> description [
	^'deprecated methods'
]

{ #category : #testing }
ClyDeprecatedMethods >> selectsMethod: aMethod [
	^aMethod isDeprecated
]
