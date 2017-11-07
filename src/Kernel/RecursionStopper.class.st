"
The RecursionStopper provides an easy way to check if we are in a recursion and execute code just once in a recursion.

RecursionStopper during:  aBlock.

executes a block just once in a recursion.

A RecursionStopper object contains a collection of active methods which are currently called from within RecrusionStopper>>#during: this means that Recursion stopper can be used multiple places without one blocking the other, but multiple stoppers cannot be nested in the same method.
"
Class {
	#name : #RecursionStopper,
	#superclass : #Object,
	#instVars : [
		'activeMethods'
	],
	#classVars : [
		'Default'
	],
	#category : #Kernel-Processes
}

{ #category : #api }
RecursionStopper class >> default [

	^ Default ifNil: [ Default := self new ]
]

{ #category : #api }
RecursionStopper class >> during: aBlock [
	<debuggerCompleteToSender>
	self default 
		stopMethod: thisContext sender method 
		during: aBlock
]

{ #category : #initialization }
RecursionStopper >> initialize [

	super initialize.
	
	activeMethods := OrderedCollection new.
]

{ #category : #operating }
RecursionStopper >> stopMethod: aMethod during: aBlock [

	(activeMethods includes: aMethod) ifTrue: [ ^ self ].
	
	activeMethods add: aMethod.
	
	aBlock ensure: [ activeMethods remove: aMethod ]
]
