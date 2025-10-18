"
In addition to storing a metaclass, I also know how to serialize and materialize the additional state of TraitedMetaclass.
"
Class {
	#name : 'FLTraitedMetaclassCluster',
	#superclass : 'FLMetaclassCluster',
	#category : 'Fuel-Core-Clusters',
	#package : 'Fuel-Core',
	#tag : 'Clusters'
}

{ #category : 'private' }
FLTraitedMetaclassCluster >> instanceVariablesStringFor: aClass [
	^ ' ' join: (aClass classLayout visibleSlots collect: [ :slot |
		slot name ])
]

{ #category : 'serialize/materialize' }
FLTraitedMetaclassCluster >> materializeInstanceWith: aDecoder [
	^ TraitedMetaclass new
]

{ #category : 'serialize/materialize' }
FLTraitedMetaclassCluster >> materializeReferencesOf: aMetaclass with: aDecoder [
	super
		materializeReferencesOf: aMetaclass
		with: aDecoder.

	aMetaclass
		baseLocalMethods: aDecoder nextEncodedReference;
		localMethodDict: aDecoder nextEncodedReference;
		baseComposition: aDecoder nextEncodedReference;
		fuelSetTraitComposition: aDecoder nextEncodedReference
]

{ #category : 'analyzing' }
FLTraitedMetaclassCluster >> referencesOf: aMetaclass do: aBlock [
	super
		referencesOf: aMetaclass
		do: aBlock.
	
	aBlock
		value: aMetaclass baseLocalMethods;
		value: aMetaclass localMethodDict;
		value: aMetaclass baseComposition;
		value: aMetaclass basicTraitComposition
]

{ #category : 'serialize/materialize' }
FLTraitedMetaclassCluster >> serializeReferencesOf: aMetaclass with: anEncoder [

	super serializeReferencesOf: aMetaclass with: anEncoder.

	anEncoder
		encodeReferenceTo: aMetaclass baseLocalMethods;
		encodeReferenceTo: aMetaclass localMethodDict;
		encodeReferenceTo: aMetaclass baseComposition;
		encodeReferenceTo: aMetaclass basicTraitComposition
]
