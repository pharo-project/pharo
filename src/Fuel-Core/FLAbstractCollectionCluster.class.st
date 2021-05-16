"
A FLAbstractCollectionCluster is the common behavior for all cluster collections. 
"
Class {
	#name : #FLAbstractCollectionCluster,
	#superclass : #FLObjectCluster,
	#category : #'Fuel-Core-Clusters-Optionals'
}

{ #category : #analyzing }
FLAbstractCollectionCluster class >> clusterBucketIn: aClusterization [

	"During the materialization references step, we may send, for example, #hash to the newly materialized objects. #hash can be implemented delegating to OTHER objects which may not have been materialized yet (I mean, its references may not have been set yet). So in this case, we need to let these clusters at the end."
	^aClusterization postBaselevelBucket
]

{ #category : #'serialize/materialize' }
FLAbstractCollectionCluster >> afterMaterializationStepWith: aDecoder [  

	objects do: [ :anObject | anObject fuelAfterMaterialization ]
]

{ #category : #'serialize/materialize' }
FLAbstractCollectionCluster >> materializeInstanceWith: aDecoder [

	^theClass new: aDecoder nextEncodedUint32
]

{ #category : #'serialize/materialize' }
FLAbstractCollectionCluster >> materializeReferencesOf: anObject with: aDecoder [
	"Hook method"

	self subclassResponsibility
]

{ #category : #'serialize/materialize' }
FLAbstractCollectionCluster >> materializeReferencesStepWith: aDecoder [

	objects do: [ :anObject | self materializeReferencesOf: anObject with: aDecoder ]
]

{ #category : #'serialize/materialize' }
FLAbstractCollectionCluster >> serializeInstance: anObject with: anEncoder [
	anEncoder encodeUint32: anObject size
]

{ #category : #'serialize/materialize' }
FLAbstractCollectionCluster >> serializeReferencesOf: anObject with: anEncoder [
	| refs |
	refs := self references
		at: anObject
		ifAbsent: [ ^ self ].
	
	anEncoder encodeUint32: refs first.

	refs
		from: 2
		to: refs size
		do: [ :ref | anEncoder encodeReferenceTo: ref ]
	
]

{ #category : #'serialize/materialize' }
FLAbstractCollectionCluster >> serializeReferencesStepWith: anEncoder [

	objects do: [ :anObject | self serializeReferencesOf: anObject with: anEncoder ]
]
