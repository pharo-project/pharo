"
I am a cluster for simple objects who define serialization and materialization via hooks on their classes.
"
Class {
	#name : #FLHookPrimitiveCluster,
	#superclass : #FLPrimitiveCluster,
	#instVars : [
		'theClass'
	],
	#category : #Fuel-Clusters
}

{ #category : #'instance creation' }
FLHookPrimitiveCluster class >> newAnalyzing: aClass [

	^ self basicNew
		initializeAnalyzing: aClass;
		yourself.
]

{ #category : #serialize/materialize }
FLHookPrimitiveCluster >> clusterMaterializeStepWith: aMaterialization [

	super clusterMaterializeStepWith: aMaterialization.
	theClass := aMaterialization decoder nextEncodedReference.

]

{ #category : #analyzing }
FLHookPrimitiveCluster >> clusterReferencesDo: aBlock [

	aBlock value: theClass
]

{ #category : #serialize/materialize }
FLHookPrimitiveCluster >> clusterSerializeStepWith: aSerialization [

	super clusterSerializeStepWith: aSerialization.
	aSerialization encoder encodeReferenceToClusterObjectClass: theClass.	
]

{ #category : #initialize-release }
FLHookPrimitiveCluster >> initializeAnalyzing: aClass [

	self initializeAnalyzing.
	theClass := aClass
]

{ #category : #serialize/materialize }
FLHookPrimitiveCluster >> materializeInstanceWith: aDecoder [
	
	^theClass materializeFrom: aDecoder
]

{ #category : #printing }
FLHookPrimitiveCluster >> printNameOn: aStream [
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."
	
	super printNameOn: aStream.
	aStream nextPut: $[.
	theClass printOn: aStream.
	aStream nextPut: $].
]

{ #category : #serialize/materialize }
FLHookPrimitiveCluster >> serializeInstance: anObject with: anEncoder [

	anObject serializeOn: anEncoder
]
