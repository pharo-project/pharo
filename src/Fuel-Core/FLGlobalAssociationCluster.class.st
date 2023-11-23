"
I am a cluster for variable bindings that belong to Smalltalk global dictionary.

For example, 

	Smalltalk globals associationAt: #Transcript 
	
could be serialized and materialized by me.
"
Class {
	#name : 'FLGlobalAssociationCluster',
	#superclass : 'FLGlobalCluster',
	#category : 'Fuel-Core-Clusters-Global',
	#package : 'Fuel-Core',
	#tag : 'Clusters-Global'
}

{ #category : 'serialize/materialize' }
FLGlobalAssociationCluster >> materializeInstanceWith: aDecoder [

	^self materializeGlobalAssociationFrom: aDecoder
]

{ #category : 'serialize/materialize' }
FLGlobalAssociationCluster >> serializeInstance: anAssociation with: anEncoder [

	self serializeGlobalAssociationKeyed: anAssociation key on: anEncoder
]
