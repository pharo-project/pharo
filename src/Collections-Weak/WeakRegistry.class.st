"
This kind of WeakRegistry using a new VM feature,
which allows a more robust finalization support.

In contrast to old implementation, it doesn't spending linear time , checking what elements became garbage.
"
Class {
	#name : #WeakRegistry,
	#superclass : #Object,
	#category : #'Collections-Weak-Registration'
}

{ #category : #accessing }
WeakRegistry class >> default [

	^ FinalizationRegistry default
]

{ #category : #testing }
WeakRegistry class >> isDeprecated [

	^ true
]

{ #category : #'instance creation' }
WeakRegistry class >> new [
	^ FinalizationRegistry new
]

{ #category : #'instance creation' }
WeakRegistry class >> new: n [

	^ FinalizationRegistry new
]
