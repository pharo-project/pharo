"
I represent a Random number generator that can be shaded between different code.

I wrap the access to my parent's functionality (#next, #nextInteger: and #next:into:)  through a mutex making me safe for multi threaded use.

To access the default shared random number generator, do:

    SharedRandom globalGenerator.
	
In principle it is better to use a shared generator since multiple users will create a more random pattern.

The system wide global shared random generator will be reset at system start up (and when the image is saved).

"
Class {
	#name : 'SharedRandom',
	#superclass : 'Random',
	#instVars : [
		'mutex'
	],
	#classInstVars : [
		'global'
	],
	#category : 'Random-Core-Base',
	#package : 'Random-Core',
	#tag : 'Base'
}

{ #category : 'accessing' }
SharedRandom class >> globalGenerator [
	"Return the system wide global shared random generator."

	^ global ifNil: [ global := self new ]
]

{ #category : 'class initialization' }
SharedRandom class >> initialize [

	SessionManager default registerSystemClassNamed: self name
]

{ #category : 'class initialization' }
SharedRandom class >> reset [
	global := nil
]

{ #category : 'system startup' }
SharedRandom class >> startUp [
	self reset
]

{ #category : 'initialization' }
SharedRandom >> initialize [
	super initialize.
	mutex := Semaphore forMutualExclusion
]

{ #category : 'accessing' }
SharedRandom >> next [
	^ mutex critical: [ super next ]
]

{ #category : 'accessing' }
SharedRandom >> next: anInteger into: anArray [
    ^ mutex critical: [ super next: anInteger into: anArray ]
]

{ #category : 'accessing' }
SharedRandom >> nextInteger: anInteger [
    ^ mutex critical: [ super nextInteger: anInteger ]
]
