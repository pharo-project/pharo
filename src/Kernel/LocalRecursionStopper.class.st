"
The LocalRecursionStopper provides an easy way to check if we are in a recursion and execute code just once in a recursion.

LocalRecursionStopper during:  aBlock.

executes a block just once in a recursion in current process. If aBlock fork new process which call same code then LocalRecursionStopper wil not stop it. That's the difference to RecursionStopper which works globally.

A LocalRecursionStopper value is a collection of active methods which are currently called from within LocalRecrusionStopper>>#during: this means that recursion stopper can be used multiple places without one blocking the other
"
Class {
	#name : #LocalRecursionStopper,
	#superclass : #ProcessLocalVariable,
	#category : #Kernel-Processes
}

{ #category : #private }
LocalRecursionStopper class >> activeMethods [
	^self value ifNil: [ self value: IdentitySet new. self value ]
]

{ #category : #api }
LocalRecursionStopper class >> during: aBlock [
	<debuggerCompleteToSender>
	self stopMethod: thisContext sender method 	during: aBlock
]

{ #category : #private }
LocalRecursionStopper class >> stopMethod: aMethod during: aBlock [

	| activeMethods |
	activeMethods := self activeMethods.
	(activeMethods includes: aMethod) ifTrue: [ ^ self ].
	
	activeMethods add: aMethod.
	
	aBlock ensure: [ activeMethods remove: aMethod ]
]
