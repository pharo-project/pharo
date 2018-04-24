"
Hermes Files contains one or more exported elements in it.
Each exported element is a subclass of myself.

I basically implements the logic to compare two elements
The comparison and the hash code is implemented using the instance variables in the objects.

In my class side I have the method #readFrom: aHermesStream
that is used to read any of my subclasses.
Each of my subclasses knows how to read itself from the stream.
"
Class {
	#name : #HEAbstractExported,
	#superclass : #Object,
	#category : #'Hermes-Model'
}

{ #category : #'instance creation' }
HEAbstractExported class >> readFrom: aReader [
	^ self new
		readFrom: aReader;
		yourself
]

{ #category : #comparing }
HEAbstractExported >> = anObject [
	"The comparison is implemented using a set of my instance variables.
	Each subclass has the responsibility to tell the instance variables to use."
	self species = anObject species
		ifFalse: [ ^ false ].
	^ self valuesToCompareForEquality = anObject valuesToCompareForEquality
]

{ #category : #comparing }
HEAbstractExported >> hash [
	"The hash is calculated with the instance variable used for comparison"
	^ self valuesToCompareForEquality
		inject: self species hash
		into: [ :hash :comparisonValue | (hash bitXor: comparisonValue hash) hashMultiply ]
]

{ #category : #comparing }
HEAbstractExported >> instVarNamesForEqualityComparison [
	"By default all the instance variables are used in the comparison and in the hash calculation"
	^ self class allInstVarNames
]

{ #category : #comparing }
HEAbstractExported >> valuesToCompareForEquality [
	"By default all the instance variables are used in the comparison and in the hash calculation"
	^ self instVarNamesForEqualityComparison collect: [ :ivName | self instVarNamed: ivName ]
]
