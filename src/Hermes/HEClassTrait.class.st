"
I represent a ClassTrait.
I am stored as a symbol.

When serialized I get the name of the base trait.
When deserialized I find the classTrait in the target environment
"
Class {
	#name : #HEClassTrait,
	#superclass : #HESymbol,
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEClassTrait class >> tag [
	^ 22
]

{ #category : #converting }
HEClassTrait >> asLiteralIn: env [
	^ (env classNamed: value) classTrait
]

{ #category : #accessing }
HEClassTrait >> value: aClassTrait [
	value := aClassTrait baseTrait name.
]
