"
I am a cluster for those Metaclasses and ClassTrait who are not stored in detail.
"
Class {
	#name : #FLGlobalClassSideCluster,
	#superclass : #FLGlobalCluster,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #'serialize/materialize' }
FLGlobalClassSideCluster >> materializeInstanceWith: aDecoder [

	^(self materializeGlobalClassFrom: aDecoder) classSide
]

{ #category : #'serialize/materialize' }
FLGlobalClassSideCluster >> serializeInstance: aClassOrTraitClassSide with: anEncoder [

	self serializeGlobalClass: aClassOrTraitClassSide instanceSide on: anEncoder

]
