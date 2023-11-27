"
I am a cluster for objects with weak indexable variables. I only serialize a weak reference if there is also a strong reference from within the graph, as otherwise the garbage collector would immediately collect the object during materialization.
"
Class {
	#name : 'FLWeakVariableObjectCluster',
	#superclass : 'FLVariableObjectCluster',
	#category : 'Fuel-Core-Clusters',
	#package : 'Fuel-Core',
	#tag : 'Clusters'
}

{ #category : 'analyzing' }
FLWeakVariableObjectCluster >> clusterReferencesDo: aBlock [
	"Ensures that nil could be encoded, later in references step."

	super clusterReferencesDo: aBlock.
	aBlock value: nil
]

{ #category : 'serialize/materialize' }
FLWeakVariableObjectCluster >> serializeReferencesVariablePartOf: anObject with: anEncoder [
	
	1 to: anObject basicSize do: [ :index |
		anEncoder encodeWeakReferenceTo: (anObject basicAt: index) ]
]

{ #category : 'analyzing' }
FLWeakVariableObjectCluster >> variablePartReferencesOf: anObject do: aBlock [ 
	"Since the variable part of Weak classes are weak references, we should not do nothing here."
]
