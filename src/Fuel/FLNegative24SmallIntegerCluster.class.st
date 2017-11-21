"
A cluster of unsigned int 24bit
"
Class {
	#name : #FLNegative24SmallIntegerCluster,
	#superclass : #FLSmallIntegerCluster,
	#category : #Fuel-Clusters
}

{ #category : #serialize/materialize }
FLNegative24SmallIntegerCluster >> materializeInstanceWith: aDecoder [
	
	^ aDecoder nextEncodedUint24 negated 
]

{ #category : #serialize/materialize }
FLNegative24SmallIntegerCluster >> serializeInstance: anInteger with: anEncoder [
	
	anEncoder encodeUint24: anInteger abs
]
