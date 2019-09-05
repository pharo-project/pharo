"
Unit tests for collection arithmetic
"
Class {
	#name : #CollectionArithmeticTest,
	#superclass : #TestCase,
	#category : #'Collections-Arithmetic-Tests'
}

{ #category : #tests }
CollectionArithmeticTest >> testAverage [
	| collection |
	collection := #(1 2 3).
	self assert: collection average equals: 2
]

{ #category : #tests }
CollectionArithmeticTest >> testAverageIfEmpty [
	| collection |
	collection := #(1 2 3 4).
	self assert: (collection averageIfEmpty: [ 0 ]) equals: 2.5
]

{ #category : #tests }
CollectionArithmeticTest >> testAverageIfEmptyWithEmptyArray [
	self assert: (#() averageIfEmpty: [ 0 ]) equals: 0
]

{ #category : #tests }
CollectionArithmeticTest >> testAverageIfEmptyWithEmptyCollection [
	self should: [#() averageIfEmpty: [ CollectionIsEmpty signal: 'Collection empty' ]] raise: CollectionIsEmpty
]

{ #category : #tests }
CollectionArithmeticTest >> testAverageWithEmptyArray [
	self should: [ #() average ] raise: CollectionIsEmpty
]

{ #category : #tests }
CollectionArithmeticTest >> testAverageWithEmptyArrayShouldRaiseExecption [
	self should: [ #() average ] raise: CollectionIsEmpty
]

{ #category : #tests }
CollectionArithmeticTest >> testAverageWithEmptyDictionary [
	self should: [ Dictionary new average ] raise: CollectionIsEmpty
]

{ #category : #tests }
CollectionArithmeticTest >> testAverageWithEmptyOrderedCollection [
	self should: [ OrderedCollection new average ] raise: CollectionIsEmpty
]

{ #category : #tests }
CollectionArithmeticTest >> testAverageWithEmptySet [
	self should: [ Set new average ] raise: CollectionIsEmpty
]
