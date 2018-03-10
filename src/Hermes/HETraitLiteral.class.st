"
I represent a Trait.
I am stored as a symbol.

When serialized I get the name of the trait.
When deserialized I find the trait in the target environment
"
Class {
	#name : #HETraitLiteral,
	#superclass : #HESymbol,
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HETraitLiteral class >> tag [
	^ 19
]

{ #category : #converting }
HETraitLiteral >> asLiteralIn: env [
	^ env classNamed: value
]

{ #category : #accessing }
HETraitLiteral >> value: aTrait [
	value := aTrait name.
]
