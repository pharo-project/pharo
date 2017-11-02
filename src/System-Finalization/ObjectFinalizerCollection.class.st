"
An ObjectFinalizerCollection is simple collection which is intended to hold ObjectFinalizers and be used by WeakRegistry.
"
Class {
	#name : #ObjectFinalizerCollection,
	#superclass : #OrderedCollection,
	#category : 'System-Finalization'
}

{ #category : #finalization }
ObjectFinalizerCollection >> finalize [
	"Finalize all elements in this collection. The elements are expected to be ObjectFinalizers, but can be any object which understands #finalize."

	self do: #finalize
]

{ #category : #testing }
ObjectFinalizerCollection >> hasMultipleExecutors [
	^ true
]
