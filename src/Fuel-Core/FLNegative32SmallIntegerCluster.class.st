"
A cluster of unsigned int 32bit
"
Class {
	#name : 'FLNegative32SmallIntegerCluster',
	#superclass : 'FLSmallIntegerCluster',
	#category : 'Fuel-Core-Clusters',
	#package : 'Fuel-Core',
	#tag : 'Clusters'
}

{ #category : 'serialize/materialize' }
FLNegative32SmallIntegerCluster >> materializeInstanceWith: aDecoder [
	
	^ aDecoder nextEncodedUint32 negated
]

{ #category : 'serialize/materialize' }
FLNegative32SmallIntegerCluster >> serializeInstance: anInteger with: anEncoder [
	
	anEncoder encodeUint32: anInteger abs
]
