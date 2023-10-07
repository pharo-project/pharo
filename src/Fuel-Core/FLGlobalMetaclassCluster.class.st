"
I am a cluster for those Metaclasses and ClassTrait who are not stored in detail.
"
Class {
	#name : 'FLGlobalMetaclassCluster',
	#superclass : 'FLGlobalCluster',
	#category : 'Fuel-Core-Clusters-Global',
	#package : 'Fuel-Core',
	#tag : 'Clusters-Global'
}

{ #category : 'serialize/materialize' }
FLGlobalMetaclassCluster >> materializeInstanceWith: aDecoder [

	^(self materializeGlobalClassFrom: aDecoder) classSide
]

{ #category : 'serialize/materialize' }
FLGlobalMetaclassCluster >> serializeInstance: aClassOrTraitClassSide with: anEncoder [

	self serializeGlobalClass: aClassOrTraitClassSide instanceSide on: anEncoder
]
