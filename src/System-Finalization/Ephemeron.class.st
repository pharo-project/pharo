"
I represent ephemeric key-value objects. Ephemerons are key-value objects (subclasses of Association) with special semantics during garbage collection.  My special behavior can resumed as follows:

- The garbage collection will iterate my instances only if the key is not referenced strongly by another object.
- Then, if no strong references to the key are found, then the values of this ephemeron are hold weakly.
- Otherwise, the values are hold strongly.

In this implementation, an Ephemeron can hold more than one value, which are all treated in the same manner. This ephemeron instance knows its container, which allows the ephemeron to remove itself from a container (such as a Dictionary) upon finalization.

!! Example usages

In general terms, do not use myself directly. Use instead an Ephemeric container like EphemeronRegistry. An Ephemeron registry will guarantee the collection of keys and values of the object inside the Ephemeron.

Otherwise, if you want to use it, you can create an Ephemeron as any association:

ephemeron := Ephemeron key: aKey value: aValue.
ephemeron container: aContainer.

!! Ephemeron Finalization

When an ephemeron's key is hold strongly just by the ephemeron itself, the Ephemeron will be mourned (finalized). That means that the VM will:
- put the Ephemeron in the mourning queue waiting for the image to take care of mourning
- make the Ephemeron non ephemeric. That is, the ephemeron instance cannot be reused.

On the image side, the finalization process will send the message #mourn to an Ephemeron.  #mourn will #finalize the Ephemeron's key, and remove the Ephemeron from it's container to allow its collection during a subsequent garbage collection.

!! More Documentation

You can read the associated paper to understand better the semantics of ephemerons:

Ephemerons: A New Finalization Mechanism. Barry Hayes. OOPSLA '97
"
Class {
	#name : #Ephemeron,
	#superclass : #Association,
	#type : #ephemeron,
	#instVars : [
		'container'
	],
	#category : #System-Finalization
}

{ #category : #'instance creation' }
Ephemeron class >> key: key value: value container: aContainer [

	^ self new
		key: key;
		value: value;
		container: aContainer;
		yourself
]

{ #category : #accessing }
Ephemeron >> container: anEphemeronContainer [

	container := anEphemeronContainer
]

{ #category : #mourning }
Ephemeron >> mourn [
	"The key is only referenced by myself.
	Finalize the key and remove myself from my container.
	
	This Ephemeron instance is not ephemeric anymore: it cannot be reused."

	key finalize.
	container ifNotNil: [ container removeEphemeron: self ].
]
