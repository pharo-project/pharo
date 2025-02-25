"
I am a specialized cluster for ExternalAddress (FFI).
"
Class {
	#name : 'FLExternalAddressCluster',
	#superclass : 'FLByteObjectCluster',
	#category : 'Fuel-Core-Clusters',
	#package : 'Fuel-Core',
	#tag : 'Clusters'
}

{ #category : 'serialize/materialize' }
FLExternalAddressCluster >> materializeInstanceWith: aDecoder [
	| size |
	size := aDecoder nextEncodedUint32.
	^ (theClass basicNew: size)
		beNull;
		yourself
]

{ #category : 'serialize/materialize' }
FLExternalAddressCluster >> serializeInstance: anExternalAddress with: anEncoder [
	anEncoder encodeUint32: anExternalAddress basicSize
]
