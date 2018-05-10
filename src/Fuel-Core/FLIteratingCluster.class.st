"
I am a template class whose algorithm for serialize a collection of objects consists on delegate the serialization of each individual object to the subclass.
"
Class {
	#name : #FLIteratingCluster,
	#superclass : #FLCluster,
	#instVars : [
		'objects',
		'references'
	],
	#category : #'Fuel-Core-Clusters'
}

{ #category : #analyzing }
FLIteratingCluster >> add: anObject traceWith: aAnalysis [
	"Add an object to the cluster and trace references."

	objects addIfNotPresent: anObject ifPresentDo: [ ^ self ].

	self 
		referencesOf: anObject 
		do: [ :aChild || actual |
			actual := aChild fuelReplacement.
			self addReferenceFrom: anObject to: actual.
			aAnalysis trace: actual ]
]

{ #category : #analyzing }
FLIteratingCluster >> addReferenceFrom: anObject to: anotherObject [
	| list |
	list := self references
		at: anObject
		ifAbsentPut: [ OrderedCollection new ].

	list add: anotherObject
]

{ #category : #analyzing }
FLIteratingCluster >> addReferencesFrom: anObject to: aCollection [
	aCollection do: [ :ref |
		self addReferenceFrom: anObject to: ref ]
]

{ #category : #'serialize/materialize' }
FLIteratingCluster >> clusterMaterializeStepWith: aMaterialization [

	super clusterMaterializeStepWith: aMaterialization.
	objects := Array new: aMaterialization decoder nextEncodedPositiveInteger
]

{ #category : #'serialize/materialize' }
FLIteratingCluster >> clusterSerializeStepWith: aSerialization [

	super clusterSerializeStepWith: aSerialization.
	aSerialization encoder encodePositiveInteger: objects size.
]

{ #category : #'initialize-release' }
FLIteratingCluster >> initializeAnalyzing [

	super initializeAnalyzing.
	objects := self newAnalyzingCollection.
]

{ #category : #'serialize/materialize' }
FLIteratingCluster >> materializeInstanceWith: aDecoder [
	"Hook method that each subclass should customize for its special way of materializing"

	^ self subclassResponsibility
]

{ #category : #'serialize/materialize' }
FLIteratingCluster >> materializeInstancesStepWith: aDecoder [

	1 to: objects size do: [ :index |
		objects 
			at: index 
			put: (self materializeInstanceWith: aDecoder) ].
]

{ #category : #analyzing }
FLIteratingCluster >> newAnalyzingCollection [
	"Answer a collection for the objects that correspond to this cluster."

	^ FLLargeIdentitySet new
]

{ #category : #accessing }
FLIteratingCluster >> objects [

	^objects
]

{ #category : #printing }
FLIteratingCluster >> printNameOn: aStream [

	super printOn: aStream
]

{ #category : #printing }
FLIteratingCluster >> printOn: aStream [

	self printNameOn: aStream.
	aStream nextPutAll: '->'.
	objects printElementsOn: aStream
]

{ #category : #accessing }
FLIteratingCluster >> references [
	^ references ifNil: [  references := FLLargeIdentityDictionary new ]
]

{ #category : #analyzing }
FLIteratingCluster >> referencesOf: anObject do: aBlock [
	"Evaluate a block with each object referenced by anObject"
]

{ #category : #'serialize/materialize' }
FLIteratingCluster >> registerIndexesOn: anEncoder [
	| indexes |
	indexes := anEncoder objectsIndexes.
	self objects do: [ :instance |
		indexes
			at: instance
			put: indexes size + 1 ]
]

{ #category : #'serialize/materialize' }
FLIteratingCluster >> serializeInstance: anObject with: anEncoder [
	"Hook method that each subclass should customize for its special way of serializing"
]

{ #category : #'serialize/materialize' }
FLIteratingCluster >> serializeInstancesStepWith: anEncoder [

	objects do: [ :instance | self serializeInstance: instance with: anEncoder ]
]
