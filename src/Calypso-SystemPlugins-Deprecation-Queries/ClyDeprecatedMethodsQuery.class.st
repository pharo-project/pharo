"
I return all deprecated methods from my scope
"
Class {
	#name : #ClyDeprecatedMethodsQuery,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-Deprecation-Queries'
}

{ #category : #printing }
ClyDeprecatedMethodsQuery >> description [
	^'deprecated methods'
]

{ #category : #testing }
ClyDeprecatedMethodsQuery >> selectsMethod: aMethod [
	^aMethod isDeprecated
]
