"
A FLDictionaryCollectionCluster is a cluster that rather than using the default (variable object) serialization, uses #keysAndValuesDo: to iterate objects and #add: during materialization. This way we avoid analyzing/serializing lots of nil. Also, the rehash is not needed. So far we use it for Dictionary, IdentityDictionary and MethodDictionary.
"
Class {
	#name : 'FLDictionaryCollectionCluster',
	#superclass : 'FLAbstractCollectionCluster',
	#category : 'Fuel-Core-Clusters-Optimized',
	#package : 'Fuel-Core',
	#tag : 'Clusters-Optimized'
}

{ #category : 'serialize/materialize' }
FLDictionaryCollectionCluster >> encodeExtraInformationForReferencesFor: aDictionary with: anEncoder [
	anEncoder encodeByte: (self hasRealAssociations: aDictionary) asBit
]

{ #category : 'testing' }
FLDictionaryCollectionCluster >> hasRealAssociations: aDictionary [
	"Weak assocations and ephemerons should not be serialized as simple key / value tuples
	but with their actual class.

	Only check the first association, assume that all other entries
	are of the same type"
	aDictionary associationsDo: [ :assoc |
		^ assoc class == Association ].
	
	^ true
]

{ #category : 'serialize/materialize' }
FLDictionaryCollectionCluster >> materializeAssociationsOf: aDictionary from: aDecoder [
	aDecoder nextEncodedUint32 timesRepeat: [
		aDictionary add: aDecoder nextEncodedReference ]
]

{ #category : 'serialize/materialize' }
FLDictionaryCollectionCluster >> materializeKeysAndValuesOf: aDictionary from: aDecoder [
	aDecoder nextEncodedUint32 timesRepeat: [
		aDictionary add: aDecoder nextEncodedReference -> aDecoder nextEncodedReference ]
]

{ #category : 'serialize/materialize' }
FLDictionaryCollectionCluster >> materializeReferencesOf: aDictionary with: aDecoder [
	aDecoder nextEncodedByte = true asBit
		ifTrue: [
			self
				materializeKeysAndValuesOf: aDictionary
				from: aDecoder ]
		ifFalse: [
			self
				materializeAssociationsOf: aDictionary
				from: aDecoder ]
]

{ #category : 'analyzing' }
FLDictionaryCollectionCluster >> referencesOf: aDictionary do: aBlock [	
	(self hasRealAssociations: aDictionary)
		ifTrue: [
			aDictionary keysAndValuesDo: [ :key :value |
				aBlock
					value: key;
					value: value ] ]
		ifFalse: [ aDictionary associationsDo: aBlock ]
]
