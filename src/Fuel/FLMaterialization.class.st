"
I implement the algorithm for materializing an object graph on a stream. FLMaterializer known how to build instances of me.
"
Class {
	#name : #FLMaterialization,
	#superclass : #Object,
	#instVars : [
		'clusterCount',
		'clusters',
		'root',
		'decoder',
		'header'
	],
	#category : #'Fuel-Core'
}

{ #category : #'instance creation' }
FLMaterialization class >> with: aDecoder [

	^self basicNew
		initializeWith: aDecoder;
		yourself

]

{ #category : #header }
FLMaterialization >> additionalObjectAt: aKey [
	^ header additionalObjectAt: aKey
]

{ #category : #private }
FLMaterialization >> afterMaterializationStep [

	clusters do: [ :aCluster | aCluster afterMaterializationStepWith: decoder ]
]

{ #category : #private }
FLMaterialization >> clusterInstancesStep [

	| aCluster |
	aCluster := decoder nextEncodedClusterClass newMaterializing.
	aCluster clusterMaterializeStepWith: self.
	aCluster materializeInstancesStepWith: decoder.
	self registerAll: aCluster objects.
	aCluster materializePostInstancesStepWith: decoder.
	clusters add: aCluster.
]

{ #category : #accessing }
FLMaterialization >> decoder [
	^ decoder
]

{ #category : #accessing }
FLMaterialization >> header: aHeader [
	header := aHeader
	
]

{ #category : #private }
FLMaterialization >> headerStep [

	decoder decodeYourself.
	clusterCount := decoder nextEncodedPositiveInteger.
	clusters := OrderedCollection new: clusterCount.
	clusters resetTo: 1. "Hack that avoids OrderedCollection>>makeRoomAtLast"
]

{ #category : #'initialize-release' }
FLMaterialization >> initializeWith: aDecoder [

	self initialize.
	decoder := aDecoder.

]

{ #category : #private }
FLMaterialization >> instancesStep [

	clusterCount timesRepeat: [ self clusterInstancesStep ]

]

{ #category : #accessing }
FLMaterialization >> objects [
	"Answer a collection with the materialized objects."

	^ decoder objects 
]

{ #category : #private }
FLMaterialization >> referencesStep [

	clusters do: [ :aCluster | aCluster materializeReferencesStepWith: decoder ]
]

{ #category : #private }
FLMaterialization >> registerAll: materializedObjects [
	
	decoder registerAll: materializedObjects 
]

{ #category : #accessing }
FLMaterialization >> root [

	^ root 
]

{ #category : #materializing }
FLMaterialization >> run [

	self headerStep.
	self instancesStep.
	self referencesStep.
	self trailerStep.
	self afterMaterializationStep.
]

{ #category : #private }
FLMaterialization >> trailerStep [

	root := decoder nextEncodedReference
]
