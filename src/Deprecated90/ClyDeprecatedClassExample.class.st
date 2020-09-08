"
I am example of deprecated class.
I just implement #isDeprecated as true
"
Class {
	#name : #ClyDeprecatedClassExample,
	#superclass : #Object,
	#category : #'Deprecated90-Calypso'
}

{ #category : #testing }
ClyDeprecatedClassExample class >> isDeprecated [
	^true
]
