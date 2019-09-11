"
I represent the user environment variables. See  `man environ` for more details.
Get access using: 

	Smalltalk os environment

I have a Dictionary-like API to access the system environment variables.
The common denominator for all platforms implements a simple API that uses strings.
In other words, methods

- #at:
- #at:put:

and its variants receive normal strings and decide wether they have to encode those strings to platform bytes or not depending on the platform.

My subclasses may or may not provide additional APIs to have more control on the particular encoding used.
"
Class {
	#name : #OSEnvironment,
	#superclass : #Object,
	#instVars : [
		'platform'
	],
	#classVars : [
		'Current'
	],
	#category : #'System-OSEnvironments-Base'
}

{ #category : #'instance creation' }
OSEnvironment class >> current [
	^ Current ifNil: [ Current := self environmentFor: OSPlatform current ]
]

{ #category : #'private - accessing' }
OSEnvironment class >> environmentFor: aPlatform [
	| environmentClass |
	
	environmentClass := self allSubclasses
		detect: [ :each | each isDefaultFor: aPlatform ]
		ifNone: [ nil ].
		
	^ (environmentClass notNil and: [ environmentClass isAvailable  ])  
		ifTrue: [ environmentClass platform: aPlatform ]
		ifFalse: [ PlatformIndependentEnvironment platform: aPlatform ]
]

{ #category : #examples }
OSEnvironment class >> example [

	Smalltalk os environment asDictionary inspect
]

{ #category : #'initialize-release' }
OSEnvironment class >> initialize [
	SessionManager default
		registerSystemClassNamed: self name
]

{ #category : #testing }
OSEnvironment class >> isAvailable [
	self flag: #pharoTodo. "Replace this for a check of FFI available"
	self environment at: #FFICalloutAPI ifAbsent: [ ^ false ].
	^ true
]

{ #category : #testing }
OSEnvironment class >> isDefaultFor: aPlatform [
	^ false
]

{ #category : #'instance creation' }
OSEnvironment class >> platform: anOSPlatform [
	^ self basicNew initializeWith: anOSPlatform 
]

{ #category : #'system startup' }
OSEnvironment class >> startUp: isImageStarting [

	isImageStarting ifTrue: [ Current := nil ]
]

{ #category : #converting }
OSEnvironment >> asDictionary [
	| dictionary |
	dictionary := Dictionary new.
	self keysAndValuesDo: [ :key :value |
		dictionary at: key put: value ].
	^ dictionary
]

{ #category : #accessing }
OSEnvironment >> associations [
	"Answer a Collection containing the receiver's associations."
	
	^Array streamContents: [ :stream |
		self associationsDo: [ :each | stream nextPut: each ]]
]

{ #category : #enumeration }
OSEnvironment >> associationsDo: aBlock [
	^ self keysAndValuesDo: [ :key :value |
		aBlock value: key -> value ]
]

{ #category : #accessing }
OSEnvironment >> at: aKey [
	"Gets the value of an environment variable called `aKey`.
	Throws a KeyNotFound exception if not found.
	It is the system reponsibility to manage the encodings of the argument and return values.
	
	This is the common denominator API for all platforms.
	Rationale: Windows does not (compared to *nix systems) provide a encoded byte representation of the value. Windows has instead its own wide string representation."

	^ self at: aKey ifAbsent: [ KeyNotFound signalFor: aKey ]
]

{ #category : #accessing }
OSEnvironment >> at: aKey ifAbsent: aBlock [
	"Gets the value of an environment variable called `aKey`.
	Execute aBlock if absent.
	It is the system reponsibility to manage the encodings of the argument and return values.
	
	This is the common denominator API for all platforms.
	Rationale: Windows does not (compared to *nix systems) provide a encoded byte representation of the value. Windows has instead its own wide string representation."
	
	self subclassResponsibility
]

{ #category : #accessing }
OSEnvironment >> at: aKey ifAbsentPut: aBlock [ 
	"Gets the value of an environment variable called `aKey`.
	If absent, insert the value given by aBlock.
	It is the system reponsibility to manage the encodings of the argument and return values.
	
	This is the common denominator API for all platforms.
	Rationale: Windows does not (compared to *nix systems) provide a encoded byte representation of the value. Windows has instead its own wide string representation."

	^ self at: aKey ifAbsent: [ self at: aKey put: aBlock value ]
]

{ #category : #accessing }
OSEnvironment >> at: aKey ifPresent: aBlock [
	"Gets the value of an environment variable called `aKey` and invoke aBlock with it.
	Return nil if absent.
	It is the system reponsibility to manage the encodings of the argument and return values.
	
	This is the common denominator API for all platforms.
	Rationale: Windows does not (compared to *nix systems) provide a encoded byte representation of the value. Windows has instead its own wide string representation."

	^ aBlock value: (self at: aKey ifAbsent: [ ^ nil ])
]

{ #category : #accessing }
OSEnvironment >> at: aKey ifPresent: presentBlock ifAbsent: absentBlock [
	"Gets the value of an environment variable called `aKey`.
	Call presentBlock with it if present.
	Execute absentBlock if absent.
	It is the system reponsibility to manage the encodings of the argument and return values.
	
	This is the common denominator API for all platforms.
	Rationale: Windows does not (compared to *nix systems) provide a encoded byte representation of the value. Windows has instead its own wide string representation."
	
	self at: aKey ifPresent: [ :v | ^ presentBlock cull: v ].
	^absentBlock value
]

{ #category : #accessing }
OSEnvironment >> at: aKey put: aValue [
	"Sets the value of an environment variable called `aKey` to `aValue`.
	It is the system reponsibility to manage the encodings of both arguments.
	
	This is the common denominator API for all platforms.
	Rationale: Windows does not (compared to *nix systems) provide a encoded byte representation of the value. Windows has instead its own wide string representation."

	^ self subclassResponsibility
]

{ #category : #enumeration }
OSEnvironment >> do: aBlock [

	^self valuesDo: aBlock
]

{ #category : #accessing }
OSEnvironment >> getEnv: aKey [
	"Gets the value of an environment variable called `aKey`.
	Returns nil if not found."
	self
		deprecated: 'Use #at:ifAbsent: instead'
		on: '16 January 2019'
		in: #Pharo7
		transformWith: '`@receiver getEnv: `@arg' 
						-> '`@receiver at: `@arg ifAbsent: [ nil ]'.

 	^ self at: aKey ifAbsent: [ nil ]
]

{ #category : #testing }
OSEnvironment >> includes: anObject [

	self do: [:each | anObject = each ifTrue: [^true]].
	^false
]

{ #category : #testing }
OSEnvironment >> includesKey: aKey [
	^ self at: aKey ifPresent: [ :value | true ] ifAbsent: [ false ].
]

{ #category : #initialization }
OSEnvironment >> initializeWith: anOSPlatform [
	super initialize.
	platform := anOSPlatform
]

{ #category : #accessing }
OSEnvironment >> keys [
	"Answer an Array containing the receiver's keys."
	
	^Array streamContents: [ :s | self keysDo: [ :key | s nextPut: key]]
]

{ #category : #enumeration }
OSEnvironment >> keysAndValuesDo: aBlock [
	self subclassResponsibility
]

{ #category : #enumeration }
OSEnvironment >> keysAndValuesDo: aBlock withAssociationString: associationString [
	| equalsIndex |
	equalsIndex := associationString indexOf: $=.
	aBlock 
		value: (associationString first: equalsIndex-1) 
		value: (associationString allButFirst: equalsIndex).
		
]

{ #category : #enumeration }
OSEnvironment >> keysDo: aBlock [
	^ self keysAndValuesDo: [ :key :value |
		aBlock value: key ]
]

{ #category : #accessing }
OSEnvironment >> platform [
	^ platform
]

{ #category : #accessing }
OSEnvironment >> removeKey: aKey [
	"Removes the entry `aKey` from the environment variables.
	It is the system reponsibility to manage the encoding of the argument.
	
	This is the common denominator API for all platforms.
	Rationale: Windows does not (compared to *nix systems) provide a encoded byte representation of the value. Windows has instead its own wide string representation."

	^ self subclassResponsibility
]

{ #category : #accessing }
OSEnvironment >> setEnv: nameString value: valueString [
	"This method calls the the platform specific set environment routine"

	^ self subclassResponsibility
]

{ #category : #accessing }
OSEnvironment >> unsetEnv: string [
	"This method calls the the platform specific unset environment routine"

	^ self ffiCall: #(int unsetenv #(String string)) module: LibC
]

{ #category : #accessing }
OSEnvironment >> values [
	"Answer a Collection containing the receiver's values."
	^ Array streamContents: [ :stream|
		self valuesDo: [ :value | stream nextPut: value ]]
]

{ #category : #enumeration }
OSEnvironment >> valuesDo: aBlock [
	^ self keysAndValuesDo: [ :key :value |
		aBlock value: value ]
]
