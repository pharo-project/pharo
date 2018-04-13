"
unsigned, 24-bit integer
"
Class {
	#name : #FLPositive24SmallIntegerCluster,
	#superclass : #FLSmallIntegerCluster,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #'serialize/materialize' }
FLPositive24SmallIntegerCluster >> materializeInstanceWith: aDecoder [
	
	^ aDecoder nextEncodedUint24 
]

{ #category : #'serialize/materialize' }
FLPositive24SmallIntegerCluster >> serializeInstance: anInteger with: anEncoder [
	
	anEncoder encodeUint24: anInteger
]
