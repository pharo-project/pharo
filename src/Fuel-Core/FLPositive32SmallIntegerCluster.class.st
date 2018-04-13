"
unsigned, 32-bit integer
"
Class {
	#name : #FLPositive32SmallIntegerCluster,
	#superclass : #FLSmallIntegerCluster,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #'serialize/materialize' }
FLPositive32SmallIntegerCluster >> materializeInstanceWith: aDecoder [

	^ aDecoder nextEncodedUint32
]

{ #category : #'serialize/materialize' }
FLPositive32SmallIntegerCluster >> serializeInstance: anInteger with: anEncoder [

	anEncoder encodeUint32: anInteger
]
