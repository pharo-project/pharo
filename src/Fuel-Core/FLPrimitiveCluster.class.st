"
I am a cluster that serializes and materializes interacting directly with the stream.
"
Class {
	#name : #FLPrimitiveCluster,
	#superclass : #FLIteratingCluster,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #analyzing }
FLPrimitiveCluster class >> clusterBucketIn: aClusterization [

	^aClusterization primitivesBucket 
]

{ #category : #analyzing }
FLPrimitiveCluster >> add: anObject traceWith: anAnalysis [
	"Add an object to the cluster. We know the object doesn't have references."

	objects addLast: anObject
]

{ #category : #analyzing }
FLPrimitiveCluster >> newAnalyzingCollection [
	"Answer a collection for the objects that correspond to this cluster."

	^OrderedCollection new
]

{ #category : #'serialize/materialize' }
FLPrimitiveCluster >> registerIndexesOn: anEncoder [

	self flag: #pharoTodo. "Converting objects collection here in this method is a bit confusing. This is because since this cluster is for primitives, they do not have pointers to other objects. Hence, instead of storing them in a IdentitySet we can use an OrderedCollection and then just convert them at the end. For more details see FLPrimitiveCluster >> #add: anObject traceWith: aAnalysis"
	objects := objects asIdentitySet.
	super registerIndexesOn: anEncoder
]
