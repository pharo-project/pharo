"
unsigned, 8-bit integer
"
Class {
	#name : #FLPositive8SmallIntegerCluster,
	#superclass : #FLSmallIntegerCluster,
	#category : #'Fuel-Clusters'
}

{ #category : #'serialize/materialize' }
FLPositive8SmallIntegerCluster >> materializeInstanceWith: aDecoder [

	^ aDecoder nextEncodedUint8
]

{ #category : #'serialize/materialize' }
FLPositive8SmallIntegerCluster >> serializeInstance: anInteger with: anEncoder [

	anEncoder encodeUint8: anInteger
]
