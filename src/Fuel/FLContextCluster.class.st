"
I am a cluster for MethodContexts.
"
Class {
	#name : #FLContextCluster,
	#superclass : #FLVariableObjectCluster,
	#category : #'Fuel-Clusters'
}

{ #category : #'serialize/materialize' }
FLContextCluster >> materializeInstanceWith: aDecoder [

	^ theClass newFromFrameSize: aDecoder nextEncodedByte

]

{ #category : #'serialize/materialize' }
FLContextCluster >> serializeInstance: aMethodContext with: anEncoder [

	anEncoder encodeByte: aMethodContext method frameSize.

]
