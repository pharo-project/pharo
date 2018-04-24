"
I am the generic cluster for storing and loading variable byte objects.
"
Class {
	#name : #FLByteObjectCluster,
	#superclass : #FLBitsObjectCluster,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #'serialize/materialize' }
FLByteObjectCluster >> materializeInstanceWith: aDecoder [

	| instance size |
	size := aDecoder nextEncodedPositiveInteger.
	instance := theClass basicNew: size.
	aDecoder nextEncodedBytesInto: instance.
	^ instance

]

{ #category : #'serialize/materialize' }
FLByteObjectCluster >> serializeInstance: anObject with: anEncoder [

	anEncoder encodePositiveInteger: anObject basicSize.
	anEncoder encodeBytes: anObject.
]
