"
I have the common behavior for generic storing and loading variable bits objects.
"
Class {
	#name : #FLBitsObjectCluster,
	#superclass : #FLObjectCluster,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #analyzing }
FLBitsObjectCluster >> add: anObject traceWith: aAnalysis [
	"Add an object to the cluster. We know the object doesn't have references."

	objects addLast: anObject
]

{ #category : #analyzing }
FLBitsObjectCluster >> newAnalyzingCollection [
	"Answer a collection for the objects that correspond to this cluster."

	^OrderedCollection new
]

{ #category : #'serialize/materialize' }
FLBitsObjectCluster >> registerIndexesOn: anEncoder [

	self flag: #pharoTodo. "Converting objects collection here in this method is a bit confusing. This is because since this cluster is for primitives, they do not have pointers to other objects. Hence, instead of storing them in a IdentitySet we can use an OrderedCollection and then just convert them at the end. For more details see FLBitsObjectCluster >> #add: anObject traceWith: aAnalysis"
	objects := objects asIdentitySet.
	super registerIndexesOn: anEncoder
]
