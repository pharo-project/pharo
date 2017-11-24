"
I am a specialized OSEnvironment version for *nix systems (Linux, OSX).
"
Class {
	#name : #UnixEnvironment,
	#superclass : #OSEnvironment,
	#category : #'System-OSEnvironments'
}

{ #category : #testing }
UnixEnvironment class >> isDefaultFor: aPlatform [ 
	^  aPlatform isUnix 
		or: [ aPlatform isMacOSX 
		or: [ aPlatform isMacOS ] ]
]

{ #category : #accessing }
UnixEnvironment >> environ [
	"Return the address of the array holding the environment variables"
	^ FFIExternalArray 
		fromPointer: (ExternalAddress loadSymbol: 'environ' from: LibC)
		type: String
]

{ #category : #private }
UnixEnvironment >> environAt: index [
	^ self environ at: index
]

{ #category : #enumeration }
UnixEnvironment >> keysAndValuesDo: aBlock [
	| index associationString  |
	index := 1	.
	[ 
		associationString := self environAt: index.
		associationString ifNil: [ ^ self ].
		self keysAndValuesDo: aBlock withAssociationString: associationString.
		index := index + 1
	] repeat.
]

{ #category : #private }
UnixEnvironment >> setEnv: nameString value: valueString [
	"This method calls the Standard C Library getenv() function"
	^ self ffiCall: #( int setenv (String nameString, String valueString, 1) ) module: LibC
]
