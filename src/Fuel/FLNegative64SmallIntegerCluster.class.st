"
A cluster of unsigned int 64bit
"
Class {
	#name : #FLNegative64SmallIntegerCluster,
	#superclass : #FLSmallIntegerCluster,
	#category : #'Fuel-Clusters'
}

{ #category : #'serialize/materialize' }
FLNegative64SmallIntegerCluster >> materializeInstanceWith: aDecoder [
	
	^ aDecoder nextEncodedUint64 negated 
]

{ #category : #'serialize/materialize' }
FLNegative64SmallIntegerCluster >> serializeInstance: anInteger with: anEncoder [
	
	anEncoder encodeUint64: anInteger abs
]
