"
I implement a generic way of serializing and materializing an object, for those that do not need more special way of doing it.
"
Class {
	#name : #FLObjectCluster,
	#superclass : #FLIteratingCluster,
	#instVars : [
		'theClass'
	],
	#category : 'Fuel-Core-Clusters'
}

{ #category : #analyzing }
FLObjectCluster class >> clusterBucketIn: aClusterization [

	^aClusterization baselevelBucket 
]

{ #category : #'instance creation' }
FLObjectCluster class >> newAnalyzing: aClass [

	^ self basicNew
		initializeAnalyzing: aClass;
		yourself.
]

{ #category : #'serialize/materialize' }
FLObjectCluster >> clusterMaterializeStepWith: aMaterialization [

	super clusterMaterializeStepWith: aMaterialization.
	theClass := aMaterialization decoder nextEncodedReference.

]

{ #category : #analyzing }
FLObjectCluster >> clusterReferencesDo: aBlock [

	aBlock value: theClass
]

{ #category : #'serialize/materialize' }
FLObjectCluster >> clusterSerializeStepWith: aSerialization [

	super clusterSerializeStepWith: aSerialization.
	aSerialization encoder encodeReferenceToClusterObjectClass: theClass
]

{ #category : #'initialize-release' }
FLObjectCluster >> initializeAnalyzing: aClass [

	self initializeAnalyzing.
	theClass := aClass
]

{ #category : #printing }
FLObjectCluster >> printNameOn: aStream [
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."
	
	super printNameOn: aStream.
	aStream nextPut: $[.
	theClass printOn: aStream.
	aStream nextPut: $].
	
]

{ #category : #printing }
FLObjectCluster >> theClass [ 
	^ theClass
]
