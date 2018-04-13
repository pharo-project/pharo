"
I am a cluster for known objects that belong to Smalltalk global dictionary.

For example, Transcript could be serialized and materialized by me.
"
Class {
	#name : #FLGlobalValueCluster,
	#superclass : #FLGlobalCluster,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #'serialize/materialize' }
FLGlobalValueCluster >> materializeInstanceWith: aDecoder [

	^(self materializeGlobalAssociationFrom: aDecoder) value
]

{ #category : #'serialize/materialize' }
FLGlobalValueCluster >> serializeInstance: anObject with: anEncoder [
	
	self serializeGlobalAssociationKeyed: (anEncoder globalEnvironment keyAtValue: anObject) on: anEncoder
]
