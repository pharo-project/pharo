"
A shared pool represents a set of bindings which are accessible to all classes which import the pool in its 'pool dictionaries'. SharedPool is NOT a dictionary but rather a name space. Bindings are represented by 'class variables' - as long as we have no better way to represent them at least.
"
Class {
	#name : 'SharedPool',
	#superclass : 'Object',
	#category : 'Kernel-Classes',
	#package : 'Kernel',
	#tag : 'Classes'
}

{ #category : 'name lookup' }
SharedPool class >> bindingOf: varName [
	"Answer the binding of some variable resolved in the scope of the receiver"
	<reflection: 'Class structural inspection - Variable lookup'>
	^ (self localBindingOf: varName) ifNil: [ self superclass bindingOf: varName ]
]

{ #category : 'name lookup' }
SharedPool class >> bindingsDo: aBlock [
	self
		deprecated:
		'use classVariables do: instead'
		transformWith: '`@receiver bindingsDo: `@arg1' -> '`@receiver classVariables do: `@arg1'.
	^self classVariables do: aBlock
]

{ #category : 'name lookup' }
SharedPool class >> classBindingOf: varName [
	"For initialization messages grant the regular scope"
	<reflection: 'Class structural inspection - Variable lookup'>
	^super bindingOf: varName
]

{ #category : 'testing' }
SharedPool class >> includesKey: aName [
	"does this pool include aName"
	^(self localBindingOf: aName) isNotNil
]

{ #category : 'testing' }
SharedPool class >> isPool [
	^true
]

{ #category : 'testing' }
SharedPool class >> isUsed [
	<reflection: 'Class structural inspection - Class kind testing'>
	^super isUsed or: [self poolUsers isNotEmpty]
]

{ #category : 'name lookup' }
SharedPool class >> localBindingOf: varName [
	"Answer the binding of some variable resolved in the scope of the receiver without accessing the superclass"
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

{ #category : 'queries' }
SharedPool class >> methodsAccessingPoolVariables [
	"Answer all methods that use one of the variables defined by the shared pool"
	^ self classVariables flatCollect: [ :variable | variable usingMethods ]
]

{ #category : 'queries' }
SharedPool class >> poolUsers [
	"Answer all classes that uses the shared pool"

	^ self environment allClasses select: [ :class | class includesSharedPoolNamed: self name ]
]

{ #category : 'queries' }
SharedPool class >> usingMethods [
	"Answer all methods that reference the Pool the variables defined by the shared pool"
	<reflection: 'Structural queries on methods - Class references'>
	^super usingMethods, self methodsAccessingPoolVariables
]
