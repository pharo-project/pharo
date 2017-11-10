"
I represent a Random number generator that can be shaded between different code.

I wrap the access to my parent's functionality (#next, #nextInt: and #next:into:)  through a mutex making me safe for multi threaded use.

To access the default shared random number generator, do:

	SharedRandom globalGenerator.
	
In principle it is better to use a shared generator since multiple users will create a more random pattern.
"
Class {
	#name : #SharedRandom,
	#superclass : #Random,
	#instVars : [
		'mutex'
	],
	#classInstVars : [
		'global'
	],
	#category : #Random-Core
}

{ #category : #accessing }
SharedRandom class >> globalGenerator [
	^ global
]

{ #category : #initialization }
SharedRandom class >> initialize [
	"self initialize"
	global := self new.
]

{ #category : #initialization }
SharedRandom >> initialize [
	super initialize.
	mutex := Semaphore forMutualExclusion.
]

{ #category : #accessing }
SharedRandom >> next [
	^ 	mutex critical: [ super privateNextValue ].
]

{ #category : #accessing }
SharedRandom >> next: anInteger into: anArray [
    ^ mutex critical: [ super next: anInteger into: anArray ].
]

{ #category : #accessing }
SharedRandom >> nextInt: anInteger [
    ^ mutex critical: [ super nextInt: anInteger ].
]
