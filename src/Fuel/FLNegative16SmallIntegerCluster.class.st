"
A cluster of unsigned int 16bit
"
Class {
	#name : #FLNegative16SmallIntegerCluster,
	#superclass : #FLSmallIntegerCluster,
	#category : 'Fuel-Clusters'
}

{ #category : #serialize/materialize }
FLNegative16SmallIntegerCluster >> materializeInstanceWith: aDecoder [
	
	^ aDecoder nextEncodedUint16 negated 
]

{ #category : #serialize/materialize }
FLNegative16SmallIntegerCluster >> serializeInstance: anInteger with: anEncoder [
	
	anEncoder encodeUint16: anInteger abs
]
