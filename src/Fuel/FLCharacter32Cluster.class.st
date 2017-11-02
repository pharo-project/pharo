"
I'm a cluster for Unicode characters.
"
Class {
	#name : #FLCharacter32Cluster,
	#superclass : #FLPrimitiveCluster,
	#category : 'Fuel-Clusters'
}

{ #category : #serialize/materialize }
FLCharacter32Cluster >> materializeInstanceWith: aDecoder [

	^ Character value: aDecoder nextEncodedUint32
]

{ #category : #serialize/materialize }
FLCharacter32Cluster >> serializeInstance: aCharacter with: anEncoder [

	anEncoder encodeUint32: aCharacter asciiValue
]
