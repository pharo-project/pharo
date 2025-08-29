"
I represent the user environment variables. See  `man environ` for more details.
Get access using: 

	Smalltalk os environment

I have a Dictionary-like API to access the system environment variables.
The common denominator for all platforms implements a simple API that uses strings.
In other words, methods

- #at:
- #at:put:

and its variants receive normal strings and decide whether they have to encode those strings to platform bytes or not depending on the platform.
"
Class {
	#name : 'OSEnvironment',
	#superclass : 'Object',
	#instVars : [
		'platform'
	],
	#classVars : [
		'Current'
	],
	#category : 'System-OSEnvironments-Base',
	#package : 'System-OSEnvironments',
	#tag : 'Base'
}

{ #category : 'instance creation' }
OSEnvironment class >> current [
	^ Current ifNil: [ Current := self platform: OSPlatform current ]
]

{ #category : 'examples' }
OSEnvironment class >> example [

	Smalltalk os environment asDictionary inspect
]

{ #category : 'class initialization' }
OSEnvironment class >> initialize [
	SessionManager default
		registerSystemClassNamed: self name
]

{ #category : 'instance creation' }
OSEnvironment class >> platform: anOSPlatform [
	^ self basicNew initializeWith: anOSPlatform
]

{ #category : 'system startup' }
OSEnvironment class >> startUp: isImageStarting [

	isImageStarting ifTrue: [ Current := nil ]
]

{ #category : 'converting' }
OSEnvironment >> asDictionary [
	| dictionary |
	dictionary := Dictionary new.
	self keysAndValuesDo: [ :key :value |
		dictionary at: key put: value ].
	^ dictionary
]

{ #category : 'accessing' }
OSEnvironment >> associations [
	"Answer a Collection containing the receiver's associations."

	^Array streamContents: [ :stream |
		self associationsDo: [ :each | stream nextPut: each ]]
]

{ #category : 'enumeration' }
OSEnvironment >> associationsDo: aBlock [
	^ self keysAndValuesDo: [ :key :value |
		aBlock value: key -> value ]
]

{ #category : 'accessing' }
OSEnvironment >> at: aKey [
	"Gets the value of an environment variable called `aKey`.
	Throws a KeyNotFound exception if not found.
	It is the system responsibility to manage the encodings of the argument and return values.

	This is the common denominator API for all platforms.
	Rationale: Windows does not (compared to *nix systems) provide a encoded byte representation of the value. Windows has instead its own wide string representation."

	^ self at: aKey ifAbsent: [ KeyNotFound signalFor: aKey ]
]

{ #category : 'accessing' }
OSEnvironment >> at: aKey ifAbsent: aBlock [
	"Gets the value of an environment variable called `aKey`.
	Execute aBlock if absent.
	It is the system responsibility to manage the encodings of the argument and return values.

	This is the common denominator API for all platforms.
	Rationale: Windows does not (compared to *nix systems) provide a encoded byte representation of the value. Windows has instead its own wide string representation."

	^ self platform environmentVariableNamed: aKey ifAbsent: aBlock
]

{ #category : 'accessing' }
OSEnvironment >> at: aKey ifAbsentPut: aBlock [
	"Gets the value of an environment variable called `aKey`.
	If absent, insert the value given by aBlock.
	It is the system responsibility to manage the encodings of the argument and return values.

	This is the common denominator API for all platforms.
	Rationale: Windows does not (compared to *nix systems) provide a encoded byte representation of the value. Windows has instead its own wide string representation."

	^ self at: aKey ifAbsent: [ self at: aKey put: aBlock value ]
]

{ #category : 'accessing' }
OSEnvironment >> at: aKey ifPresent: aBlock [
	"Gets the value of an environment variable called `aKey` and invoke aBlock with it.
	Return nil if absent.
	It is the system responsibility to manage the encodings of the argument and return values.

	This is the common denominator API for all platforms.
	Rationale: Windows does not (compared to *nix systems) provide a encoded byte representation of the value. Windows has instead its own wide string representation."

	^ aBlock value: (self at: aKey ifAbsent: [ ^ nil ])
]

{ #category : 'accessing' }
OSEnvironment >> at: aKey ifPresent: presentBlock ifAbsent: absentBlock [
	"Gets the value of an environment variable called `aKey`.
	Call presentBlock with it if present.
	Execute absentBlock if absent.
	It is the system responsibility to manage the encodings of the argument and return values.

	This is the common denominator API for all platforms.
	Rationale: Windows does not (compared to *nix systems) provide a encoded byte representation of the value. Windows has instead its own wide string representation."

	self at: aKey ifPresent: [ :v | ^ presentBlock cull: v ].
	^absentBlock value
]

{ #category : 'accessing' }
OSEnvironment >> at: aKey put: aValue [
	"Sets the value of an environment variable called `aKey` to `aValue`.
	It is the system responsibility to manage the encodings of both arguments.

	This is the common denominator API for all platforms.
	Rationale: Windows does not (compared to *nix systems) provide a encoded byte representation of the value. Windows has instead its own wide string representation."

	^ self platform environmentVariableNamed: aKey put: aValue
]

{ #category : 'enumeration' }
OSEnvironment >> do: aBlock [

	^self valuesDo: aBlock
]

{ #category : 'testing' }
OSEnvironment >> includes: anObject [

	self do: [:each | anObject = each ifTrue: [^true]].
	^false
]

{ #category : 'testing' }
OSEnvironment >> includesKey: aKey [
	^ self at: aKey ifPresent: [ :value | true ] ifAbsent: [ false ]
]

{ #category : 'initialization' }
OSEnvironment >> initializeWith: anOSPlatform [
	super initialize.
	platform := anOSPlatform
]

{ #category : 'accessing' }
OSEnvironment >> keys [
	"Answer an Array containing the receiver's keys."

	^Array streamContents: [ :s | self keysDo: [ :key | s nextPut: key]]
]

{ #category : 'enumeration' }
OSEnvironment >> keysAndValuesDo: aBlock [

	^ self platform environmentVariablesDo: aBlock
]

{ #category : 'enumeration' }
OSEnvironment >> keysDo: aBlock [
	^ self keysAndValuesDo: [ :key :value |
		aBlock value: key ]
]

{ #category : 'accessing' }
OSEnvironment >> platform [
	^ platform
]

{ #category : 'accessing' }
OSEnvironment >> removeKey: aKey [
	"Removes the entry `aKey` from the environment variables.
	It is the system responsibility to manage the encoding of the argument.

	This is the common denominator API for all platforms.
	Rationale: Windows does not (compared to *nix systems) provide a encoded byte representation of the value. Windows has instead its own wide string representation."

	^ self platform removeEnvironmentVariableNamed: aKey
]

{ #category : 'accessing' }
OSEnvironment >> setEnv: nameString value: valueString [
	"This method calls the the platform specific set environment routine"

	^ self platform setEnv: nameString value: valueString
]

{ #category : 'accessing' }
OSEnvironment >> setEnv: nameString value: valueString during: aBlock [

	| oldValue |
	oldValue := self at: nameString ifAbsent: [ nil ].
	[
	self setEnv: nameString value: valueString.
	aBlock value ] ensure: [
		oldValue
			ifNil: [ self removeKey: nameString ]
			ifNotNil: [ self setEnv: nameString value: oldValue ] ]
]

{ #category : 'accessing' }
OSEnvironment >> values [
	"Answer a Collection containing the receiver's values."
	^ Array streamContents: [ :stream|
		self valuesDo: [ :value | stream nextPut: value ]]
]

{ #category : 'enumeration' }
OSEnvironment >> valuesDo: aBlock [
	^ self keysAndValuesDo: [ :key :value |
		aBlock value: value ]
]
