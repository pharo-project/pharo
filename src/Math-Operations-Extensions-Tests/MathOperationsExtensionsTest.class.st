Class {
	#name : #MathOperationsExtensionsTest,
	#superclass : #TestCase,
	#category : #'Math-Operations-Extensions-Tests'
}

{ #category : #tests }
MathOperationsExtensionsTest >> testCloseToPrecisionSequenceableCollections [

	self assert: (#( 1.9283 2.3029 ) closeTo: #( 1.9284 2.3028 ) precision: 0.001).


	self deny: (#( 1.9283 2.3029 ) closeTo: #( 1.9284 2.3028 ) precision: 0.00001)
]

{ #category : #tests }
MathOperationsExtensionsTest >> testCloseToSequenceableCollections [

	self assert: (#( 1.9283901234902349 2.302949083493849 ) closeTo: #( 1.9283901234902348 2.302949083493899 )).

	self deny: (#( 1.9283901234902349 2.302949083493849 3 ) closeTo: #( 1.9283901234902348 3 2.302949083493899 )).

	self deny: (#( 1.9283901234902349 2.302949083493849 ) closeTo: #( 1.8283901234902348 2.302949083493899 ))
]
