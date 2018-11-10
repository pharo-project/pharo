"
I am example of deprecated class.
I just implement #isDeprecated as true
"
Class {
	#name : #ClyDeprecatedClassExample,
	#superclass : #Object,
	#category : #'Calypso-SystemPlugins-Deprecation-Queries-Tests'
}

{ #category : #testing }
ClyDeprecatedClassExample class >> isDeprecated [
	^true
]
