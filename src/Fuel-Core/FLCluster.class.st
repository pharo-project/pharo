"
I represent a cluster of objects grouped by some specific similarity. I know how to serialize and materialize them all together.
"
Class {
	#name : #FLCluster,
	#superclass : #Object,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #analyzing }
FLCluster class >> clusterBucketIn: aClusterization [
	"Answer in which clusters bucket place this cluster. This defines the sort order in which the clusters will be ordered. See FLAnalyzer >> clusteredObjects for more information."

	self subclassResponsibility 
]

{ #category : #'instance creation' }
FLCluster class >> new [
	self error: 'Use another instance creation message.'
]

{ #category : #'instance creation' }
FLCluster class >> newAnalyzing [

	^ self basicNew
		initializeAnalyzing;
		yourself.
]

{ #category : #'instance creation' }
FLCluster class >> newMaterializing [

	^ self basicNew
		initializeMaterializing;
		yourself.
]

{ #category : #'serialize/materialize' }
FLCluster >> afterMaterializationStepWith: aDecoder [

]

{ #category : #'serialize/materialize' }
FLCluster >> clusterMaterializeStepWith: aMaterialization [
	"Materialize cluster stuff"

]

{ #category : #analyzing }
FLCluster >> clusterReferencesDo: aBlock [
	"Evaluate a block with each object referenced by the cluster"
]

{ #category : #'serialize/materialize' }
FLCluster >> clusterSerializeStepWith: aSerialization [
	"Serialize cluster stuff"
]

{ #category : #hooks }
FLCluster >> fuelAccept: aGeneralMapper [
	^ aGeneralMapper visitSubstitution: self by: nil
]

{ #category : #'initialize-release' }
FLCluster >> initializeAnalyzing [

	self initialize.
]

{ #category : #'initialize-release' }
FLCluster >> initializeMaterializing [

	self initialize.
]

{ #category : #'serialize/materialize' }
FLCluster >> materializeInstancesStepWith: aDecoder [

]

{ #category : #'serialize/materialize' }
FLCluster >> materializePostInstancesStepWith: aDecoder [


]

{ #category : #'serialize/materialize' }
FLCluster >> materializeReferencesStepWith: aDecoder [

]

{ #category : #accessing }
FLCluster >> objects [
	"Answer the objects that this cluster groups."

	self subclassResponsibility
]

{ #category : #'serialize/materialize' }
FLCluster >> registerIndexesOn: anEncoder [
	"Maps objects with indexes in the dictionary."
]

{ #category : #'serialize/materialize' }
FLCluster >> serializeInstancesStepWith: anEncoder [

]

{ #category : #'serialize/materialize' }
FLCluster >> serializePostInstancesStepWith: anEncoder [

]

{ #category : #'serialize/materialize' }
FLCluster >> serializeReferencesStepWith: anEncoder [

]

{ #category : #analyzing }
FLCluster >> traceWith: anAnalysis [ 

	self clusterReferencesDo: [ :aChild | anAnalysis privateTrace: aChild ]
]
