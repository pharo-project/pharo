"
A FLSimpleCollectionCluster is a cluster that rather than using the default (variable object) serialization, uses #do: to iterate objects and #add: during materialization. So far we use it for Set, IdentitySet and OrderedCollection.
"
Class {
	#name : 'FLSimpleCollectionCluster',
	#superclass : 'FLAbstractCollectionCluster',
	#category : 'Fuel-Core-Clusters-Optimized',
	#package : 'Fuel-Core',
	#tag : 'Clusters-Optimized'
}

{ #category : 'serialize/materialize' }
FLSimpleCollectionCluster >> materializeReferencesOf: anObject with: aDecoder [
	aDecoder nextEncodedUint32 timesRepeat: [
		anObject add: aDecoder nextEncodedReference ]
]

{ #category : 'analyzing' }
FLSimpleCollectionCluster >> referencesOf: anObject do: aBlock [
	anObject do: aBlock
]
