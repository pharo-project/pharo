"
I implement the algorithm for materializing an object graph on a stream. FLMaterializer known how to build instances of me.
"
Class {
	#name : 'FLMaterialization',
	#superclass : 'Object',
	#traits : 'TFLConfigurable',
	#classTraits : 'TFLConfigurable classTrait',
	#instVars : [
		'clusterCount',
		'clusters',
		'decoder',
		'header',
		'stopBlock'
	],
	#category : 'Fuel-Core-Base',
	#package : 'Fuel-Core',
	#tag : 'Base'
}

{ #category : 'instance creation' }
FLMaterialization class >> basicRun [
	^ self new run
]

{ #category : 'instance creation' }
FLMaterialization class >> detectResponsibleMaterialization [
	^ self allSubclasses
		detect: [ :class | class isResponsible ]
		ifNone: [ self ]
]

{ #category : 'instance creation' }
FLMaterialization class >> isResponsible [
	^ self subclassResponsibility
]

{ #category : 'instance creation' }
FLMaterialization class >> run [
	^ self detectResponsibleMaterialization basicRun
]

{ #category : 'header' }
FLMaterialization >> additionalObjectAt: aKey [
	^ header additionalObjectAt: aKey
]

{ #category : 'private' }
FLMaterialization >> basicRun [
	| stream |
	self configuration hasMultipleObjects
		ifFalse: [ self runSteps ]
		ifTrue: [
			stream := self context stream.
			[ stream atEnd ] whileFalse: [
				self runSteps ] ]
]

{ #category : 'private' }
FLMaterialization >> clusterInstancesStep [

	| aCluster |
	aCluster := decoder nextEncodedClusterClass newMaterializing.
	aCluster clusterMaterializeStepWith: decoder.
	aCluster materializeInstancesStepWith: decoder.
	self registerAll: aCluster objects.
	aCluster materializePostInstancesStepWith: decoder.
	clusters add: aCluster
]

{ #category : 'private' }
FLMaterialization >> clusterPostMaterializationStep [

	clusters do: [ :aCluster | aCluster afterMaterializationStepWith: decoder ]
]

{ #category : 'private' }
FLMaterialization >> decoderHeaderStep [
	decoder decodeYourself.
	clusterCount := decoder nextEncodedUint32.
	clusters := OrderedCollection new: clusterCount.
	clusters resetTo: 1. "Hack that avoids OrderedCollection>>makeRoomAtLast"
]

{ #category : 'protected' }
FLMaterialization >> fileHeaderStep [
	self
		verifySignature;
		verifyVersion
]

{ #category : 'protected' }
FLMaterialization >> headerStep [
	header := FLHeaderMaterializer new materialize root.
	self context materializedObjects addHeader: header
]

{ #category : 'initialization' }
FLMaterialization >> initialize [
	super initialize.
	
	decoder := FLDecoder new
]

{ #category : 'private' }
FLMaterialization >> instancesStep [

	clusterCount timesRepeat: [ self clusterInstancesStep ]
]

{ #category : 'materializing' }
FLMaterialization >> postMaterializationStep [
	header executePostMaterializationActions
]

{ #category : 'materializing' }
FLMaterialization >> preMaterializationStep [
	header executePreMaterializationActions
]

{ #category : 'private' }
FLMaterialization >> referencesStep [

	clusters do: [ :aCluster | aCluster materializeReferencesStepWith: decoder ]
]

{ #category : 'private' }
FLMaterialization >> registerAll: materializedObjects [
	
	decoder registerAll: materializedObjects
]

{ #category : 'materializing' }
FLMaterialization >> run [
	"Enables early escape, e.g. when only materializing the header"
	stopBlock := [ ^ self context materializedObjects ].
	
	self
		basicRun;
		stop
]

{ #category : 'materializing' }
FLMaterialization >> runHeaderSteps [
	self
		fileHeaderStep;
		decoderHeaderStep;
		headerStep.
		
	self configuration shouldMaterializeHeaderOnly ifFalse: [ ^ self ].
	
	self stop
]

{ #category : 'materializing' }
FLMaterialization >> runSteps [
	self
		runHeaderSteps;
		preMaterializationStep;
		instancesStep;
		referencesStep;
		trailerStep;
		clusterPostMaterializationStep;
		postMaterializationStep
]

{ #category : 'materializing' }
FLMaterialization >> stop [
	stopBlock value
]

{ #category : 'private' }
FLMaterialization >> trailerStep [
	self context materializedObjects
		addRoot: decoder nextEncodedReference;
		addObjects: decoder objects
]

{ #category : 'materializing' }
FLMaterialization >> verifySignature [
	| decodedSignature |
	decodedSignature := decoder decodeSignature.
	(self configuration signature asByteArray = decodedSignature) ifFalse: [ 
		FLBadSignature 
			signalCurrentSignature: self configuration signature 
			streamSignature: decodedSignature ]
]

{ #category : 'materializing' }
FLMaterialization >> verifyVersion [
	| decodedVersion |
	decodedVersion := decoder decodeVersion.
	self configuration version major = decodedVersion major ifTrue: [
		^ self ].
	
	FLBadVersion
		signalCurrentVersion: self configuration version 
		streamVersion: decodedVersion
]
