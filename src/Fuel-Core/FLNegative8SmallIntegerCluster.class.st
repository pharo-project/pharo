"
A cluster of unsigned int 8bit
"
Class {
	#name : 'FLNegative8SmallIntegerCluster',
	#superclass : 'FLSmallIntegerCluster',
	#category : 'Fuel-Core-Clusters',
	#package : 'Fuel-Core',
	#tag : 'Clusters'
}

{ #category : 'serialize/materialize' }
FLNegative8SmallIntegerCluster >> materializeInstanceWith: aDecoder [

	^ aDecoder nextEncodedUint8 negated
]

{ #category : 'serialize/materialize' }
FLNegative8SmallIntegerCluster >> serializeInstance: anInteger with: anEncoder [

	anEncoder encodeUint8: anInteger abs
]
