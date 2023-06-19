"
I represent ephemeric key-value objects to use inside a FinalizationReigstry.
Ephemerons are key-value objects (subclasses of Association) with special semantics during garbage collection.  My special behavior can resumed as follows:

- The garbage collection will iterate my instances only if the key is not referenced strongly by another object.
- Then, if no strong references to the key are found, then the values of this ephemeron are hold weakly.
- Otherwise, the values are hold strongly.

This ephemeron instance knows its container, which allows the ephemeron to remove itself from a container (such as a Dictionary) upon finalization.

# Example usages

Do not use myself directly, I am internal to the `FinalizationRegistry`.
The `FinalizationRegistry` will guarantee the collection of keys and values of the object inside myself.

# Ephemeron Finalization

When an ephemeron's key is hold strongly just by the ephemeron itself, the Ephemeron will be mourned (finalized). That means that the VM will:
- put the Ephemeron in the mourning queue waiting for the image to take care of mourning
- make the Ephemeron non ephemeric. That is, the ephemeron instance cannot be reused.

On the image side, the finalization process will send the message #mourn to an Ephemeron.  #mourn will #finalize the Ephemeron's key, and remove the Ephemeron from it's container to allow its collection during a subsequent garbage collection.

# Implementation details

I am implemented as a double linked list, to make removals O(1)

!! More Documentation

You can read the associated paper to understand better the semantics of ephemerons:

Ephemerons: A New Finalization Mechanism. Barry Hayes. OOPSLA '97
"
Class {
	#name : #FinalizationRegistryEntry,
	#superclass : #Association,
	#type : #ephemeron,
	#instVars : [
		'container',
		'next',
		'previous'
	],
	#category : #'System-Finalization-Registry'
}

{ #category : #'instance creation' }
FinalizationRegistryEntry class >> key: key value: value container: aContainer [

	^ self new
		key: key;
		value: value;
		container: aContainer;
		yourself
]

{ #category : #accessing }
FinalizationRegistryEntry >> chainNext: aNode [
	"Insert a node in the middle of myself and my next node.
	Assume the argument has no next or previous"
	| currentNext |
	currentNext := next.
	next := aNode.
	aNode ifNil: [ ^ self ].

	aNode next: currentNext.
	currentNext ifNotNil: [
		currentNext previous: aNode ].

	aNode previous: self
]

{ #category : #accessing }
FinalizationRegistryEntry >> check [

	^ self next previous == self and: [ self previous next == self ]
]

{ #category : #accessing }
FinalizationRegistryEntry >> container [

	^ container
]

{ #category : #accessing }
FinalizationRegistryEntry >> container: anEphemeronContainer [

	container := anEphemeronContainer
]

{ #category : #testing }
FinalizationRegistryEntry >> includes: anObject [

	key = anObject ifTrue: [ ^ true ].
	next ifNil: [ ^ false ].
	^ next includes: anObject
]

{ #category : #mourning }
FinalizationRegistryEntry >> mourn [
	"The key is only referenced by myself.
	This Ephemeron instance is not ephemeric anymore: it cannot be reused.
	
	Ask the container to finalize myself"

	container finalizeEphemeron: self
]

{ #category : #accessing }
FinalizationRegistryEntry >> next [

	^ next
]

{ #category : #accessing }
FinalizationRegistryEntry >> next: anObject [

	next := anObject
]

{ #category : #accessing }
FinalizationRegistryEntry >> previous [

	^ previous
]

{ #category : #accessing }
FinalizationRegistryEntry >> previous: anObject [

	previous := anObject
]

{ #category : #accessing }
FinalizationRegistryEntry >> removeFromChain [

	previous ifNil: [ ^ self ].
	previous chainNext: next
]

{ #category : #accessing }
FinalizationRegistryEntry >> size [

	next ifNil: [ ^ 1 ].
	^ 1 + next size
]
