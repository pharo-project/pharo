"
I am a cluster for simple objects who define serialization and materialization via hooks on their classes.
"
Class {
	#name : 'FLHookPrimitiveCluster',
	#superclass : 'FLPrimitiveCluster',
	#instVars : [
		'theClass'
	],
	#category : 'Fuel-Core-Clusters',
	#package : 'Fuel-Core',
	#tag : 'Clusters'
}

{ #category : 'instance creation' }
FLHookPrimitiveCluster class >> newAnalyzing: aClass [

	^ self basicNew
		initializeAnalyzing: aClass;
		yourself.
]

{ #category : 'serialize/materialize' }
FLHookPrimitiveCluster >> clusterMaterializeStepWith: aDecoder [

	super clusterMaterializeStepWith: aDecoder.
	theClass := aDecoder nextEncodedReference
]

{ #category : 'analyzing' }
FLHookPrimitiveCluster >> clusterReferencesDo: aBlock [

	aBlock value: theClass
]

{ #category : 'serialize/materialize' }
FLHookPrimitiveCluster >> clusterSerializeStepWith: anEncoder [

	super clusterSerializeStepWith: anEncoder.
	anEncoder encodeReferenceToClusterObjectClass: theClass
]

{ #category : 'initialization' }
FLHookPrimitiveCluster >> initializeAnalyzing: aClass [

	self initializeAnalyzing.
	theClass := aClass
]

{ #category : 'serialize/materialize' }
FLHookPrimitiveCluster >> materializeInstanceWith: aDecoder [
	
	^theClass materializeFrom: aDecoder
]

{ #category : 'printing' }
FLHookPrimitiveCluster >> printNameOn: aStream [
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."
	
	super printNameOn: aStream.
	aStream nextPut: $[.
	theClass printOn: aStream.
	aStream nextPut: $].
]

{ #category : 'serialize/materialize' }
FLHookPrimitiveCluster >> serializeInstance: anObject with: anEncoder [

	anObject serializeOn: anEncoder
]
