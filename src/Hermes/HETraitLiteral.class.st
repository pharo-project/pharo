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
