"
A FLSimpleCollectionCluster is a special optional cluster that rather than using the default (variable object) serialization, uses #do: to iterate objects and #add: during materialization. So far we use it for Set, IdentitySet and OrderedCollection.
"
Class {
	#name : #FLSimpleCollectionCluster,
	#superclass : #FLAbstractCollectionCluster,
	#category : #'Fuel-Core-Clusters-Optionals'
}

{ #category : #'serialize/materialize' }
FLSimpleCollectionCluster >> materializeReferencesOf: anObject with: aDecoder [
	aDecoder nextEncodedPositiveInteger timesRepeat: [
		anObject add: aDecoder nextEncodedReference ]
]

{ #category : #analyzing }
FLSimpleCollectionCluster >> referencesOf: anObject do: aBlock [
	aBlock value: anObject size.
	anObject
		do: [ :each | aBlock value: each ]

]
