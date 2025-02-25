"
I implement the algorithm for serializing an object graph on a stream. FLSerializer known how to build instances of me.
"
Class {
	#name : 'FLSerialization',
	#superclass : 'Object',
	#traits : 'TFLConfigurable',
	#classTraits : 'TFLConfigurable classTrait',
	#instVars : [
		'encoder',
		'clusters'
	],
	#category : 'Fuel-Core-Base',
	#package : 'Fuel-Core',
	#tag : 'Base'
}

{ #category : 'instance creation' }
FLSerialization class >> basicRun [
	^ self new
		run;
		yourself
]

{ #category : 'instance creation' }
FLSerialization class >> detectResponsibleSerialization [
	^ self allSubclasses
		detect: [ :class | class isResponsible ]
		ifNone: [ self ]
]

{ #category : 'instance creation' }
FLSerialization class >> isResponsible [
	^ self subclassResponsibility
]

{ #category : 'instance creation' }
FLSerialization class >> run [
	^ self detectResponsibleSerialization basicRun
]

{ #category : 'private' }
FLSerialization >> analysisStep [

	| anAnalysis |
	anAnalysis := FLAnalysis run.
	clusters := anAnalysis clusterization clusters.
	encoder objectCount: anAnalysis clusterization objectCount.
	self possiblySubstituteRootFrom: anAnalysis
]

{ #category : 'private' }
FLSerialization >> clusterInstancesStepOf: aCluster [

	encoder encodeClusterClass: aCluster class.
	aCluster clusterSerializeStepWith: encoder.
	aCluster serializeInstancesStepWith: encoder.
	aCluster serializePostInstancesStepWith: encoder.
]

{ #category : 'accessing' }
FLSerialization >> clusters [
	^ clusters
]

{ #category : 'debugging' }
FLSerialization >> clustersSortedByAmountOfObjects [
	^ self clusters sorted: [ :a :b | a objects size > b objects size ]
]

{ #category : 'private' }
FLSerialization >> encoderHeaderStep [
	encoder
		encodeYourself;
		encodeUint32: clusters size
]

{ #category : 'private' }
FLSerialization >> fileHeaderStep [
	encoder
		encodeSignature;
		encodeVersion
]

{ #category : 'private' }
FLSerialization >> headerStep [
	"Make sure the contents of the encoder have been written to the stream
	so that the header encoder writes it's contents after those."
	encoder flush.
	
	FLHeaderSerializer new serialize
]

{ #category : 'initialization' }
FLSerialization >> initialize [
	super initialize.
	
	encoder := FLEncoder new
]

{ #category : 'private' }
FLSerialization >> instancesStep [

	clusters do: [ :aCluster | aCluster registerIndexesOn: encoder ].
	clusters do: [ :aCluster | self clusterInstancesStepOf: aCluster ]
]

{ #category : 'accessing' }
FLSerialization >> objects [
	"Answer a collection with the serialized objects."

	^ encoder objectsIndexes keys
]

{ #category : 'private' }
FLSerialization >> possiblySubstituteRootFrom: anAnalysis [
	"The root object may have been substituted. We need to update the
	object in the configuration to reflect the actual root object"
	anAnalysis clusterization substitutionsBucket
		at: FLSubstitutionCluster
		ifPresent: [ :cluster |
			cluster substitutions
				at: self configuration object
				ifPresent: [ :substitute |
					self configuration object: substitute ] ]
]

{ #category : 'private' }
FLSerialization >> prepareObjectsStep [
	clusters do: [ :aCluster |
		aCluster prepareObjectsStep ]
]

{ #category : 'private' }
FLSerialization >> referencesStep [

	clusters do: [ :aCluster | aCluster serializeReferencesStepWith: encoder ]
]

{ #category : 'serializing' }
FLSerialization >> run [
	"Serialize the graph starting at the root object received and answers the FLSerialization object"
	^ self configuration hasMultipleObjects
		ifFalse: [ self runSteps ]
		ifTrue: [
			| numberOfObjects |
			numberOfObjects := self context object size.
			self context object withIndexDo: [ :object :index |
				FLContext new
					stream: self context stream;
					object: object;
					useDuring: [ self runSteps ] ] ]
]

{ #category : 'private' }
FLSerialization >> runSteps [
	self
		analysisStep;
		prepareObjectsStep;
		fileHeaderStep;
		encoderHeaderStep;
		headerStep;
		instancesStep;
		referencesStep;
		trailerStep
]

{ #category : 'private' }
FLSerialization >> trailerStep [
	encoder encodeReferenceTo: self context object
]
