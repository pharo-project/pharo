"
A FLDictionaryCollectionCluster is a special optional cluster that rather than using the default (variable object) serialization, uses #keysAndValuesDo: to iterate objects and #add: during materialization. This way we avoid analyzing/serializing lots of nil. Also, the rehash is not needed. So far we use it for Dictionary, IdentityDictionary and MethodDictionary.
"
Class {
	#name : #FLDictionaryCollectionCluster,
	#superclass : #FLAbstractCollectionCluster,
	#category : #'Fuel-Core-Clusters-Optionals'
}

{ #category : #'serialize/materialize' }
FLDictionaryCollectionCluster >> materializeReferencesOf: anObject with: aDecoder [
	aDecoder nextEncodedPositiveInteger timesRepeat: [
		anObject add: aDecoder nextEncodedReference -> aDecoder nextEncodedReference ]
]

{ #category : #analyzing }
FLDictionaryCollectionCluster >> referencesOf: anObject do: aBlock [
	aBlock value: anObject size.
	anObject
		keysAndValuesDo: [ :key :value | 
			aBlock value: key. 
			aBlock value: value.  ]

]
