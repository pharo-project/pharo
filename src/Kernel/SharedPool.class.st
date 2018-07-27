"
A shared pool represents a set of bindings which are accessible to all classes which import the pool in its 'pool dictionaries'. SharedPool is NOT a dictionary but rather a name space. Bindings are represented by 'class variables' - as long as we have no better way to represent them at least.
"
Class {
	#name : #SharedPool,
	#superclass : #Object,
	#category : #'Kernel-Classes'
}

{ #category : #'name lookup' }
SharedPool class >> bindingOf: varName [
	"Answer the binding of some variable resolved in the scope of the receiver"
	| aSymbol binding |
	aSymbol := varName asSymbol.

	"First look in classVar dictionary."
	binding := self classPool bindingOf: aSymbol.
	binding ifNotNil:[^binding].

	"Next look in shared pools."
	self sharedPools do:[:pool | 
		binding := pool bindingOf: aSymbol.
		binding ifNotNil:[^binding].
	].

	"subclassing and environment are not preserved"
	^nil
]

{ #category : #'name lookup' }
SharedPool class >> bindingsDo: aBlock [
	^self classPool bindingsDo: aBlock
]

{ #category : #'name lookup' }
SharedPool class >> classBindingOf: varName [
	"For initialization messages grant the regular scope"
	^super bindingOf: varName
]

{ #category : #'name lookup' }
SharedPool class >> hasBindingThatBeginsWith: aString [
	"Answer true if the receiver has a binding that begins with aString, false otherwise"

	"First look in classVar dictionary."
	(self classPool hasBindingThatBeginsWith: aString) ifTrue:[^true].
	"Next look in shared pools."
	self sharedPools do:[:pool | 
		(pool hasBindingThatBeginsWith: aString) ifTrue: [^true]].
	^false
]

{ #category : #'name lookup' }
SharedPool class >> includesKey: aName [
	"does this pool include aName"
	^(self bindingOf: aName) notNil
]

{ #category : #testing }
SharedPool class >> isUsed [
	^super isUsed or: [self poolUsers isNotEmpty]
]

{ #category : #pools }
SharedPool class >> poolUsers [
	"Answer an Array of all classes that uses the shared pool"

	| aCollection |
	aCollection := ReadWriteStream on: Array new.
	self environment allClassesDo: [:class | (class includesSharedPoolNamed: self name) ifTrue: [aCollection nextPut: class]].
	^ aCollection contents
]
