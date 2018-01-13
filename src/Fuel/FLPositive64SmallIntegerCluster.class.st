"
unsigned, 64-bit integer
"
Class {
	#name : #FLPositive64SmallIntegerCluster,
	#superclass : #FLSmallIntegerCluster,
	#category : #'Fuel-Clusters'
}

{ #category : #'serialize/materialize' }
FLPositive64SmallIntegerCluster >> materializeInstanceWith: aDecoder [

	^ aDecoder nextEncodedUint64
]

{ #category : #'serialize/materialize' }
FLPositive64SmallIntegerCluster >> serializeInstance: anInteger with: anEncoder [

	anEncoder encodeUint64: anInteger
]
