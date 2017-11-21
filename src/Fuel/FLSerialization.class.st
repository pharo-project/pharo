"
I implement the algorithm for serializing an object graph on a stream. FLSerializer known how to build instances of me.
"
Class {
	#name : #FLSerialization,
	#superclass : #Object,
	#instVars : [
		'encoder',
		'root',
		'clusters',
		'analyzer'
	],
	#category : #Fuel-Core
}

{ #category : #'instance creation' }
FLSerialization class >> with: anEncoder root: anObject analyzer: anAnalyzer [
	
	^ self basicNew
		initializeWith: anEncoder
		root: anObject
		analyzer: anAnalyzer;
		yourself
]

{ #category : #private }
FLSerialization >> analysisStep [

	| anAnalysis |
	anAnalysis := analyzer analysisFor: root.
	clusters := anAnalysis clusterization clusters.
	encoder objectCount: anAnalysis clusterization objectCount

]

{ #category : #private }
FLSerialization >> clusterInstancesStepOf: aCluster [

	encoder encodeClusterClass: aCluster class.
	aCluster clusterSerializeStepWith: self.
	aCluster serializeInstancesStepWith: encoder.
	aCluster serializePostInstancesStepWith: encoder.

]

{ #category : #accessing }
FLSerialization >> clusters [
	^ clusters
]

{ #category : #debugging }
FLSerialization >> clustersSortedByAmountOfObjects [
	^ self clusters sorted: [ :a :b | a objects size > b objects size ]
]

{ #category : #accessing }
FLSerialization >> encoder [
	^ encoder
]

{ #category : #hooks }
FLSerialization >> fuelAccept: aGeneralMapper [
	^ aGeneralMapper visitSubstitution: self by: nil
]

{ #category : #private }
FLSerialization >> headerStep [
	
	encoder encodeYourself.
	encoder encodePositiveInteger: clusters size.
]

{ #category : #initialize-release }
FLSerialization >> initializeWith: anEncoder root: anObject analyzer: anAnalyzer [
			
	self initialize.
	encoder := anEncoder.
	root := anObject fuelReplacement.
	analyzer := anAnalyzer.
]

{ #category : #private }
FLSerialization >> instancesStep [

	clusters do: [ :aCluster | aCluster registerIndexesOn: encoder ].
	clusters do: [ :aCluster | self clusterInstancesStepOf: aCluster ]
]

{ #category : #accessing }
FLSerialization >> objects [
	"Answer a collection with the serialized objects."

	^ encoder objectsIndexes keys
]

{ #category : #private }
FLSerialization >> referencesStep [

	clusters do: [ :aCluster | aCluster serializeReferencesStepWith: encoder ]
]

{ #category : #accessing }
FLSerialization >> root [
	^ root
]

{ #category : #serializing }
FLSerialization >> run [
	"Serialize the graph starting at the root object."

	self analysisStep.
	self headerStep.
	self instancesStep.
	self referencesStep.
	self trailerStep.
]

{ #category : #private }
FLSerialization >> trailerStep [

	encoder encodeReferenceTo: root
]
