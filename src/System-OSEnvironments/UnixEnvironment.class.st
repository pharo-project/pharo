"
I am a specialized OSEnvironment version for *nix systems (Linux, OSX).
"
Class {
	#name : #UnixEnvironment,
	#superclass : #OSEnvironment,
	#category : #'System-OSEnvironments-Platforms'
}

{ #category : #testing }
UnixEnvironment class >> isDefaultFor: aPlatform [ 
	^  aPlatform isUnix 
		or: [ aPlatform isMacOSX 
		or: [ aPlatform isMacOS ] ]
]

{ #category : #accessing }
UnixEnvironment >> at: aKey encoding: anEncoding ifAbsent: aBlock [
	"Gets the value of an environment variable called `aKey`.
	Execute aBlock if absent.
	It is the system reponsibility to manage the encoding."
	| result |
	result := self
		rawAt: (aKey encodeWith: anEncoding)
		ifAbsent: [ ^ aBlock value].
	^ result decodeWith: anEncoding
]

{ #category : #accessing }
UnixEnvironment >> at: aKey ifAbsent: aBlock [
	"Gets the value of an environment variable called `aKey`.
	Execute aBlock if absent.
	It is the system reponsibility to manage the encoding."
	^ self at: aKey encoding: self defaultEncoding ifAbsent: aBlock
]

{ #category : #accessing }
UnixEnvironment >> at: aKey put: aValue [

	^ self at: aKey put: aValue encoding: self defaultEncoding
]

{ #category : #accessing }
UnixEnvironment >> at: aKey put: aValue encoding: anEncoding [

	^ self
		rawAt: (aKey encodeWith: anEncoding)
		put: (aValue encodeWith: anEncoding)
]

{ #category : #private }
UnixEnvironment >> basicGetEnvRaw: encodedVariableName [

	"PRIVATE: This primitive call works on Strings, while the correct way to manage encodings is with raw data.
	Use me through #getEnvRaw: to correctly marshall data."
	
	"Gets the value of an environment variable called `anEncodedVariableName` already encoded but in ByteString form."
	
	<primitive: 'primitiveGetenv' module: '' error: ec>
	ec ifNil: [ ^ (self basicGetEnvRawViaFFI: encodedVariableName asString) asByteArray].
	self primitiveFail
]

{ #category : #private }
UnixEnvironment >> basicGetEnvRawViaFFI: arg1 [

	"PRIVATE: This FFI call works on Strings, while the correct way to manage encodings is with raw data.
	Use me through #getEnvViaFFI: to correctly marshall data."

	"This method calls the Standard C Library getenv() function.
	The name of the argument (arg1) should fit decompiled version."
	
	 ^ self ffiCall: #( String getenv (String arg1) ) module: LibC
]

{ #category : #accessing }
UnixEnvironment >> defaultEncoding [

	^ ZnCharacterEncoder utf8
]

{ #category : #private }
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

{ #category : #accessing }
UnixEnvironment >> rawAt: aKey ifAbsent: aBlock [
	"Gets the value of an environment variable called `aKey`.
	Execute aBlock if absent.
	It is the system reponsibility to manage the encoding."
	
	| rawValue |
	rawValue := self basicGetEnvRaw: aKey asString.
	^ rawValue
		ifNil: [ aBlock value ]
		ifNotNil: [ rawValue asByteArray ].
]

{ #category : #accessing }
UnixEnvironment >> rawAt: anEncodedName put: bytes [
	
	^ self setEnv: anEncodedName asString value: bytes asString
]

{ #category : #accessing }
UnixEnvironment >> rawRemoveKey: key [
	^ self unsetEnv: key asString
]

{ #category : #accessing }
UnixEnvironment >> removeKey: key [
	^ self removeKey: key encoded: self defaultEncoding
]

{ #category : #accessing }
UnixEnvironment >> removeKey: key encoded: anEncoding [
	^ self rawRemoveKey: (key encodeWith: anEncoding)
]

{ #category : #private }
UnixEnvironment >> setEnv: nameString value: valueString [
	"This method calls the Standard C Library getenv() function"
	^ self ffiCall: #( int setenv (String nameString, String valueString, 1) ) module: LibC
]

{ #category : #private }
UnixEnvironment >> unsetEnv: string [
	"This method calls the Standard C Library getenv() function"
	 ^ self ffiCall: #( int unsetenv (String string) ) module: LibC
]
