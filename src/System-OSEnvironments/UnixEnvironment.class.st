"
I am a specialized OSEnvironment version for *nix systems (Linux, OSX).
See my superclass to understand my common usage.

# Encoding Management

I provide a variant of the Dictionary-like API that receives an extra argument specifying an encoding.
Valid encodings are those specified by the Zinc-Character-Encoding-* packages.
The API accepts both encoding objects and symbols which are used to lookup encodings.

For example, the following usages are valid:

OSEnvironment current at: 'HOME' encoding: #utf8.
OSEnvironment current at: 'HOME' encoding: #utf8 asZnCharacterEncoder.
OSEnvironment current at: 'HOME' encoding: ZnCharacterEncoder utf8.

# Implementation Details

I try to use a primitive to get/set environment variables, and if it fails or it is not available, I use corresponding ffi calls using byteArrays with encoded strings as argument.
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
UnixEnvironment >> at: aKey encoding: anEncoding [
	"Gets the value of an environment variable called `aKey`.
	Throws a KeyNotFound exception if not found.
	Use `anEncoding` to encode the arguments and return values.
	
	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."
	
	^ self at: aKey encoding: anEncoding ifAbsent: [ KeyNotFound signalFor: aKey ]
]

{ #category : #accessing }
UnixEnvironment >> at: aKey encoding: anEncoding ifAbsent: aBlock [
	"Gets the value of an environment variable called `aKey`.
	Execute aBlock if absent.
	Use `anEncoding` to encode the arguments and return values.
	
	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."
	
	| result |
	result := self
		rawAt: (aKey encodeWith: anEncoding)
		ifAbsent: [ ^ aBlock value].
	^ result decodeWith: anEncoding
]

{ #category : #accessing }
UnixEnvironment >> at: aKey encoding: anEncoding ifAbsentPut: aBlock [
	"Gets the value of an environment variable called `aKey`.
	If absent, insert the value given by aBlock.
	Use `anEncoding` to encode the arguments and return values.
	
	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."
	
	^ self at: aKey encoding: anEncoding ifAbsent: [ self at: aKey put: aBlock value encoding: anEncoding ]
]

{ #category : #accessing }
UnixEnvironment >> at: aKey encoding: anEncoding ifPresent: aBlock [
	"Gets the value of an environment variable called `aKey` and invoke aBlock with it.
	Return nil if absent.
	Use `anEncoding` to encode the arguments and return values.
	
	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."
	
	^ aBlock value: (self at: aKey encoding: anEncoding ifAbsent: [ ^ nil ])
]

{ #category : #accessing }
UnixEnvironment >> at: aKey encoding: anEncoding ifPresent: presentBlock ifAbsent: absentBlock [
	"Gets the value of an environment variable called `aKey`.
	Call presentBlock with it if present.
	Execute absentBlock if absent.
	Use `anEncoding` to encode the arguments and return values.
	
	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."
	
	self at: aKey encoding: anEncoding ifPresent: [ :v | ^ presentBlock cull: v ].
	^absentBlock value
]

{ #category : #accessing }
UnixEnvironment >> at: aKey ifAbsent: aBlock [
	"See super>>at:ifAbsent:.
	Uses a single encoding determined dynamically"

	^ self at: aKey encoding: self defaultEncoding ifAbsent: aBlock
]

{ #category : #accessing }
UnixEnvironment >> at: aKey put: aValue [
	"See super>>at:put:.
	Uses a single encoding determined dynamically"

	^ self at: aKey put: aValue encoding: self defaultEncoding
]

{ #category : #accessing }
UnixEnvironment >> at: aKey put: aValue encoding: anEncoding [
	"Sets the value of an environment variable called `aKey` to `aValue`.
	Use `anEncoding` to encode both arguments.
	
	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."

	^ self
		rawAt: (aKey encodeWith: anEncoding)
		put: (aValue encodeWith: anEncoding)
]

{ #category : #private }
UnixEnvironment >> basicGetEnvRaw: encodedVariableName [

	"PRIVATE: This primitive call works on Strings, while the correct way to manage encodings is with raw data.
	Use me through #rawAt: to correctly marshall data."
	
	"Gets the value of an environment variable called `anEncodedVariableName` already encoded but in ByteString form."
	
	<primitive: 'primitiveGetenv' module: '' error: ec>
	ec ifNil: [ ^ (self basicGetEnvRawViaFFI: encodedVariableName asString) asByteArray].
	self primitiveFail
]

{ #category : #private }
UnixEnvironment >> basicGetEnvRawViaFFI: arg1 [

	"PRIVATE: This FFI call works on Strings, while the correct way to manage encodings is with raw data.
	Use me through #basicGetEnvRaw: to correctly marshall data."

	"This method calls the Standard C Library getenv() function.
	The name of the argument (arg1) should fit decompiled version."
	
	 ^ self ffiCall: #( String getenv (String arg1) ) module: LibC
]

{ #category : #accessing }
UnixEnvironment >> defaultEncoding [

	^ OSPlatform current defaultEncoding
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
UnixEnvironment >> rawAt: anEncodedKey [
	"Gets the value of an environment variable called `anEncodedKey` that is already encoded (i.e., it is a byte array).
	Throws a KeyNotFound exception if not found.
	
	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."
	
	^ self rawAt: anEncodedKey ifAbsent: [ KeyNotFound signalFor: anEncodedKey ]
]

{ #category : #accessing }
UnixEnvironment >> rawAt: anEncodedKey ifAbsent: aBlock [
	"Gets the value of an environment variable called `anEncodedKey` that is already encoded (i.e., it is a byte array).
	Execute aBlock if absent.
	
	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."
	
	| rawValue |
	rawValue := self basicGetEnvRaw: anEncodedKey asString.
	^ rawValue
		ifNil: [ aBlock value ]
		ifNotNil: [ rawValue asByteArray ].
]

{ #category : #accessing }
UnixEnvironment >> rawAt: anEncodedKey ifAbsentPut: aBlock [
	"Gets the value of an environment variable called `aKey` that is already encoded (i.e., it is a byte array).
	If absent, insert the value given by aBlock.
	
	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."
	
	^ self rawAt: anEncodedKey ifAbsent: [ self rawAt: anEncodedKey put: aBlock value ]
]

{ #category : #accessing }
UnixEnvironment >> rawAt: anEncodedKey ifPresent: aBlock [
	"Gets the value of an environment variable called `anEncodedKey` that is already encoded (i.e., it is a byte array) and invoke aBlock with it.
	Return nil if absent.
	
	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."
	
	^ aBlock value: (self rawAt: anEncodedKey ifAbsent: [ ^ nil ])
]

{ #category : #accessing }
UnixEnvironment >> rawAt: anEncodedKey ifPresent: presentBlock ifAbsent: absentBlock [
	"Gets the value of an environment variable called `anEncodedKey` that is already encoded (i.e., it is a byte array).
	Call presentBlock with it if present.
	Execute absentBlock if absent.
	
	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."
	
	self rawAt: anEncodedKey ifPresent: [ :v | ^ presentBlock cull: v ].
	^absentBlock value
]

{ #category : #accessing }
UnixEnvironment >> rawAt: anEncodedKey put: someBytes [
	"Sets the value of an environment variable called `anEncodedKey` to `someBytes`.
	Both arguments should be already encoded (i.e., they are byte arrays).
	
	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."

	^ self setEnv: anEncodedKey asString value: someBytes asString
]

{ #category : #accessing }
UnixEnvironment >> rawRemoveKey: anEncodedKey [
	"Removes an environment variable called `anEncodedKey` that is already encoded (i.e., it is a byte array).
	
	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."

	^ self unsetEnv: anEncodedKey asString
]

{ #category : #accessing }
UnixEnvironment >> removeKey: key [
	"See super>>removeKey:.
	Uses a single encoding determined dynamically"
	
	^ self removeKey: key encoded: self defaultEncoding
]

{ #category : #accessing }
UnixEnvironment >> removeKey: key encoded: anEncoding [
	"Removes the entry `aKey` from the environment variables.
	Use `anEncoding` to encode the arguments.
	
	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."

	^ self rawRemoveKey: (key encodeWith: anEncoding)
]

{ #category : #private }
UnixEnvironment >> setEnv: nameString value: valueString [
	"This method calls the Standard C Library setenv() function"

	^ self ffiCall: #(int setenv #(String nameString , String valueString , 1)) module: LibC
]

{ #category : #private }
UnixEnvironment >> unsetEnv: string [
	"This method calls the Standard C Library getenv() function"
	 ^ self ffiCall: #( int unsetenv (String string) ) module: LibC
]
