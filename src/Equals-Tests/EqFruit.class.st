"
I'm a domain object used by Equals tests.

I'm a fruit with a maturity stage.
"
Class {
	#name : #EqFruit,
	#superclass : #Object,
	#traits : 'TEquality',
	#classTraits : 'TEquality classTrait',
	#instVars : [
		'stage'
	],
	#category : #'Equals-Tests'
}

{ #category : #comparing }
EqFruit >> = anObject [ 
	self species = anObject species ifFalse: [^false].
	^self valuesToCompareForEquality = anObject valuesToCompareForEquality
]

{ #category : #staging }
EqFruit >> beRipe [
	stage := #ripe
]

{ #category : #staging }
EqFruit >> beRotten [
	stage := #rotten
]

{ #category : #staging }
EqFruit >> beUnripe [
	stage := #unripe
]

{ #category : #accessing }
EqFruit >> color [
	^self subclassResponsibility
]

{ #category : #comparing }
EqFruit >> hash [
	^self valuesToCompareForEquality inject: self species hash into: [:hash :comparisonValue|
		(hash bitXor: comparisonValue hash) hashMultiply]

]

{ #category : #'initialize - release' }
EqFruit >> initialize [
	super initialize.
	self beUnripe
]
