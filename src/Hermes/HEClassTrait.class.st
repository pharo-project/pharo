Class {
	#name : #HEClassTrait,
	#superclass : #HESymbol,
	#category : 'Hermes-Model'
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
