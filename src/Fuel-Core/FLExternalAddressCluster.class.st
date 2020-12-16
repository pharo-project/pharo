Class {
	#name : #FLExternalAddressCluster,
	#superclass : #FLByteObjectCluster,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #'serialize/materialize' }
FLExternalAddressCluster >> materializeInstanceWith: aDecoder [
	| size |
	size := aDecoder nextEncodedPositiveInteger.
	^ (theClass basicNew: size)
		beNull;
		yourself

]

{ #category : #'serialize/materialize' }
FLExternalAddressCluster >> serializeInstance: anExternalAddress with: anEncoder [
	anEncoder encodePositiveInteger: anExternalAddress basicSize
]
