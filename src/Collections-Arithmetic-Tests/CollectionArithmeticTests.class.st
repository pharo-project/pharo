Class {
	#name : #CollectionArithmeticTests,
	#superclass : #TestCase,
	#category : #'Collections-Arithmetic-Tests'
}

{ #category : #tests }
CollectionArithmeticTests >> testAverage [
	| collection |
	collection := #(1 2 3).
	self assert: collection average equals: 2
]

{ #category : #tests }
CollectionArithmeticTests >> testAverageIfEmpty [
	| collection |
	collection := #(1 2 3 4).
	self assert: (collection averageIfEmpty: [ 0 ]) equals: 2.5
]

{ #category : #tests }
CollectionArithmeticTests >> testAverageIfEmptyWithEmptyArray [
	self assert: (#() averageIfEmpty: [ 0 ]) equals: 0
]

{ #category : #tests }
CollectionArithmeticTests >> testAverageIfEmptyWithEmptyCollection [
	self should: [#() averageIfEmpty: [ CollectionIsEmpty signal: 'Collection empty' ]] raise: CollectionIsEmpty
]

{ #category : #tests }
CollectionArithmeticTests >> testAverageWithEmptyArray [
	self should: [ #() average ] raise: CollectionIsEmpty
]

{ #category : #tests }
CollectionArithmeticTests >> testAverageWithEmptyArrayShouldRaiseExecption [
	self should: [ #() average ] raise: CollectionIsEmpty
]

{ #category : #tests }
CollectionArithmeticTests >> testAverageWithEmptyDictionary [
	self should: [ Dictionary new average ] raise: CollectionIsEmpty
]

{ #category : #tests }
CollectionArithmeticTests >> testAverageWithEmptyOrderedCollection [
	self should: [ OrderedCollection new average ] raise: CollectionIsEmpty
]

{ #category : #tests }
CollectionArithmeticTests >> testAverageWithEmptySet [
	self should: [ Set new average ] raise: CollectionIsEmpty
]
