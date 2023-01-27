"
In addition to storing a metaclass, I also know how to serialize and materialize the additional state of MetaclassForTraits.
"
Class {
	#name : #FLMetaclassForTraitsCluster,
	#superclass : #FLTraitedMetaclassCluster,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #'serialize/materialize' }
FLMetaclassForTraitsCluster >> materializeInstanceWith: aDecoder [
	^ MetaclassForTraits new
]
