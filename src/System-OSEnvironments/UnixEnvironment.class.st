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
UnixEnvironment >> at: aKey ifAbsent: aBlock [
	"Gets the value of an environment variable called `aKey`.
	Execute aBlock if absent.
	It is the system reponsibility to manage the encoding."
	^ (self getEnv: aKey) ifNil: aBlock
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
	ec ifNil: [ ^self getEnvViaFFI: encodedVariableName ].
	self primitiveFail
]

{ #category : #private }
UnixEnvironment >> basicGetEnvViaFFI: arg1 [

	"PRIVATE: This FFI call works on Strings, while the correct way to manage encodings is with raw data.
	Use me through #getEnvViaFFI: to correctly marshall data."

	"This method calls the Standard C Library getenv() function.
	The name of the argument (arg1) should fit decompiled version."
	
	 ^ self ffiCall: #( String getenv (String arg1) ) module: LibC
]

{ #category : #private }
UnixEnvironment >> defaultEncoding [

	^ ZnCharacterEncoder utf8
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

{ #category : #private }
UnixEnvironment >> getEnv: aVariableName [
	"Gets the value of an environment variable called `aVariableName`
	It is the system reponsibility to manage the encoding.
	Rationale: A common denominator for all platforms providing an already decoded string, because windows does not (compared to *nix systems) provide a encoded byte representation of the value. Windows has instead its own wide string representation."
	^ self getEnv: aVariableName encoding: self defaultEncoding
]

{ #category : #private }
UnixEnvironment >> getEnv: aVariableName encoding: anEncoding [
	"Gets the value of an environment variable called `` using `anEncoding` to encode/decode arguments and return values.
	Rationale: *xes could use different encodings"

	| rawValue |
	rawValue := self getEnvRaw: (aVariableName encodeWith: anEncoding).
	^ rawValue ifNotNil: [ rawValue decodeWith: anEncoding ]
]

{ #category : #private }
UnixEnvironment >> getEnvRaw: encodedVariableName [
	
	"Gets the value of an environment variable called `anEncodedVariableName` already encoded.
	It is the user responsibility to encode and decode argument and return values in the encoding of this preference.
	Rationale: Some systems may want to have the liberty to use different encodings, or even to put binary data in the variables."
	
	"This method calls the primitiveGetenv primitive and falls back into FFI if not available."
	
	"OSEnvironment current getEnvRaw: 'HOME' utf8Encoded"
	
	| rawValue |
	rawValue := self basicGetEnvRaw: encodedVariableName asString.
	^ rawValue ifNotNil: [ rawValue asByteArray ].
]

{ #category : #private }
UnixEnvironment >> getEnvViaFFI: encodedString [

	"The FFI call works on Strings, while the correct way to manage encodings is with raw data.
	Transform back and forth from byte arrays to strings and vice versa to maintain the correct behaviour"
	^ (self basicGetEnvViaFFI: encodedString asString) asByteArray
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
UnixEnvironment >> rawAt: anEncodedName put: bytes [
	
	^ self setEnv: anEncodedName asString value: bytes asString
]

{ #category : #accessing }
UnixEnvironment >> removeKey: key [
	^ self unsetEnv: key
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
