"
unsigned, 16-bit integer cluster
"
Class {
	#name : #FLPositive16SmallIntegerCluster,
	#superclass : #FLSmallIntegerCluster,
	#category : #'Fuel-Clusters'
}

{ #category : #'serialize/materialize' }
FLPositive16SmallIntegerCluster >> materializeInstanceWith: aDecoder [
	
	^ aDecoder nextEncodedUint16 
]

{ #category : #'serialize/materialize' }
FLPositive16SmallIntegerCluster >> serializeInstance: anInteger with: anEncoder [
	
	anEncoder encodeUint16: anInteger
]
