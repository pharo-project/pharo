"
I represent the class side of a trait (a MetaclassForTraits) used as a literal.
I am stored as a symbol: the name of the base trait.

When deserialized I find the class side of the trait in the target environment.
"
Class {
	#name : 'HEClassTrait',
	#superclass : 'HESymbol',
	#category : 'Hermes-Model',
	#package : 'Hermes',
	#tag : 'Model'
}

{ #category : 'accessing' }
HEClassTrait class >> tag [
	^ 22
]

{ #category : 'converting' }
HEClassTrait >> asLiteralIn: env [
	^ (env classNamed: value) classTrait
]

{ #category : 'accessing' }
HEClassTrait >> value: aClassTrait [
	value := aClassTrait instanceSide name
]
