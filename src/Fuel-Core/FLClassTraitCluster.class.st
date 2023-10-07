"
I am a cluster for storing class traits in full detail.
"
Class {
	#name : 'FLClassTraitCluster',
	#superclass : 'FLBehaviorCluster',
	#category : 'Fuel-Core-Clusters',
	#package : 'Fuel-Core',
	#tag : 'Clusters'
}

{ #category : 'analyzing' }
FLClassTraitCluster class >> clusterBucketIn: aClusterization [

	^aClusterization metalevelClassSideBucket
]

{ #category : 'serialize/materialize' }
FLClassTraitCluster >> materializeInstanceWith: aDecoder [

	^ClassTrait basicNew
]

{ #category : 'serialize/materialize' }
FLClassTraitCluster >> materializePostInstance: aClassTrait with: aDecoder [

	aClassTrait 
		initialize.
]

{ #category : 'serialize/materialize' }
FLClassTraitCluster >> serializeInstancesStepWith: anEncoder [
	"Do nothing. I know my objects have nothing to serialize in this step."
]

{ #category : 'serialize/materialize' }
FLClassTraitCluster >> serializePostInstance: aClassTrait with: anEncoder [
]
