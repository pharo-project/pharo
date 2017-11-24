Class {
	#name : #HEAbstractExported,
	#superclass : #Object,
	#category : #'Hermes-Model'
}

{ #category : #formatting }
HEAbstractExported class >> readFrom: aReader [
	^ self new
		readFrom: aReader;
		yourself
]

{ #category : #comparing }
HEAbstractExported >> = anObject [
	self species = anObject species
		ifFalse: [ ^ false ].
	^ self valuesToCompareForEquality = anObject valuesToCompareForEquality
]

{ #category : #comparing }
HEAbstractExported >> hash [
	^ self valuesToCompareForEquality
		inject: self species hash
		into: [ :hash :comparisonValue | (hash bitXor: comparisonValue hash) hashMultiply ]
]

{ #category : #comparing }
HEAbstractExported >> instVarNamesForEqualityComparison [
	^ self class allInstVarNames
]

{ #category : #comparing }
HEAbstractExported >> valuesToCompareForEquality [
	^ self instVarNamesForEqualityComparison collect: [ :ivName | self instVarNamed: ivName ]
]
