"
I am responsible for serializing and materializing ephemerons. Ephemerons are subclasses of Association with the EphemeronLayout class layout. The garbage collector finalizes ephemerons (called ""mourning"") when no strong references to the key of the association remain.

See the class comment of FinalizationRegistryEntry for additional information.

I only serialize the ephemeron's key if there's also a strong reference to it from within the graph, as otherwise the garbage collector would finalize and collect the ephemeron immediately during materialization.
"
Class {
	#name : 'FLEphemeronCluster',
	#superclass : 'FLFixedObjectCluster',
	#category : 'Fuel-Core-Clusters',
	#package : 'Fuel-Core',
	#tag : 'Clusters'
}

{ #category : 'analyzing' }
FLEphemeronCluster >> clusterReferencesDo: aBlock [
	"Ensures that nil could be encoded, later in references step."

	super clusterReferencesDo: aBlock.
	aBlock value: nil
]

{ #category : 'analyzing' }
FLEphemeronCluster >> referencesOf: anObject do: aBlock [
	"Do not store strong reference to the key, which is essentially a weak reference.
	Do not trace the key either. We only want to serialize the key if there's an
	explicit reference to it in the graph."
	| key |
	key := anObject key.
	variablesMapping
		referencesOf: anObject
		do: [ :object |
			object == key ifFalse: [
				aBlock value: object ] ]
]

{ #category : 'serialize/materialize' }
FLEphemeronCluster >> serializeReferencesOf: anObject with: anEncoder [
	"As we did not trace the key we can simply encode it here,
	provided it was strongly referenced from within the graph."
	anEncoder encodeWeakReferenceTo: anObject key.
	
	"The references do not contain the key"
	super
		serializeReferencesOf: anObject
		with: anEncoder
]
