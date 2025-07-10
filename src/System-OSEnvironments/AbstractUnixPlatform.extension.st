Extension { #name : 'AbstractUnixPlatform' }

{ #category : '*System-OSEnvironments' }
AbstractUnixPlatform >> at: aKey encoding: anEncoding ifAbsent: aBlock [
	"Gets the value of an environment variable called `aKey`.
	Execute aBlock if absent.
	Use `anEncoding` to encode the arguments and return values.

	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."

	| result |
	result := self rawAt: (aKey encodeWith: anEncoding) ifAbsent: [ ^ aBlock value ].
	^ result decodeWith: anEncoding
]

{ #category : '*System-OSEnvironments' }
AbstractUnixPlatform >> at: aKey put: aValue encoding: anEncoding [
	"Sets the value of an environment variable called `aKey` to `aValue`.
	Use `anEncoding` to encode both arguments.

	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."

	^ self rawAt: (aKey encodeWith: anEncoding) put: (aValue encodeWith: anEncoding)
]

{ #category : '*System-OSEnvironments' }
AbstractUnixPlatform >> basicGetEnvRaw: encodedVariableName [

	"PRIVATE: This primitive call works on Strings, while the correct way to manage encodings is with raw data.
	Use me through #rawAt: to correctly marshall data."

	"Gets the value of an environment variable called `anEncodedVariableName` already encoded but in ByteString form."

	<primitive: 'primitiveGetenv' module: '' error: ec>
	ec ifNil: [ ^ (self basicGetEnvRawViaFFI: encodedVariableName asString) asByteArray].
	self primitiveFail
]

{ #category : '*System-OSEnvironments' }
AbstractUnixPlatform >> basicGetEnvRawViaFFI: arg1 [

	"PRIVATE: This FFI call works on Strings, while the correct way to manage encodings is with raw data.
	Use me through #basicGetEnvRaw: to correctly marshall data."

	"This method calls the Standard C Library getenv() function.
	The name of the argument (arg1) should fit decompiled version."

	 ^ self ffiCall: #( String getenv (String arg1) ) module: LibC
]

{ #category : '*System-OSEnvironments' }
AbstractUnixPlatform >> environ [
	"Return the address of the array holding the environment variables"

	^ FFIExternalArray fromPointer: (ExternalAddress loadSymbol: 'environ' from: LibC) type: String
]

{ #category : '*System-OSEnvironments' }
AbstractUnixPlatform >> environAt: index [

	^ self environ at: index
]

{ #category : '*System-OSEnvironments' }
AbstractUnixPlatform >> environmentVariableNamed: aKey ifAbsent: aBlock [
	"See super
	Uses a single encoding determined dynamically"

	^ self at: aKey encoding: self defaultEncoding ifAbsent: aBlock
]

{ #category : '*System-OSEnvironments' }
AbstractUnixPlatform >> environmentVariableNamed: aKey put: aValue [
	"See super
	Uses a single encoding determined dynamically"

	^ self at: aKey put: aValue encoding: self defaultEncoding
]

{ #category : '*System-OSEnvironments' }
AbstractUnixPlatform >> environmentVariablesDo: aBlock [

	| index associationString |
	index := 1.
	[
	associationString := self environAt: index.
	associationString ifNil: [ ^ self ].
	self keysAndValuesDo: aBlock withAssociationString: associationString.
	index := index + 1 ] repeat
]

{ #category : '*System-OSEnvironments' }
AbstractUnixPlatform >> rawAt: anEncodedKey ifAbsent: aBlock [
	"Gets the value of an environment variable called `anEncodedKey` that is already encoded (i.e., it is a byte array).
	Execute aBlock if absent.

	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."

	| rawValue |
	rawValue := self basicGetEnvRaw: anEncodedKey asString.
	^ rawValue
		ifNil: [ aBlock value ]
		ifNotNil: [ rawValue asByteArray ]
]

{ #category : '*System-OSEnvironments' }
AbstractUnixPlatform >> rawAt: anEncodedKey put: someBytes [
	"Sets the value of an environment variable called `anEncodedKey` to `someBytes`.
	Both arguments should be already encoded (i.e., they are byte arrays).

	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."

	^ self setEnv: anEncodedKey asString value: someBytes asString
]

{ #category : '*System-OSEnvironments' }
AbstractUnixPlatform >> rawRemoveKey: anEncodedKey [
	"Removes an environment variable called `anEncodedKey` that is already encoded (i.e., it is a byte array).

	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."

	^ self unsetEnv: anEncodedKey asString
]

{ #category : '*System-OSEnvironments' }
AbstractUnixPlatform >> removeEnvironmentVariableNamed: key [
	"See super
	Uses a single encoding determined dynamically"

	^ self removeKey: key encoded: self defaultEncoding
]

{ #category : '*System-OSEnvironments' }
AbstractUnixPlatform >> removeKey: key encoded: anEncoding [
	"Removes the entry `aKey` from the environment variables.
	Use `anEncoding` to encode the arguments.

	This is a *nix specific API.
	Rationale: In *nix systems (compared to windows systems) environment variables are stored as raw bytes and can be encoded in different forms."

	^ self rawRemoveKey: (key encodeWith: anEncoding)
]

{ #category : '*System-OSEnvironments' }
AbstractUnixPlatform >> setEnv: nameString value: valueString [
	"int setenv(const char *name, const char *value, int overwrite);"

	^ self ffiCall: #( int setenv #( String nameString #, String valueString #, int 1 ) ) module: LibC
]

{ #category : '*System-OSEnvironments' }
AbstractUnixPlatform >> unsetEnv: string [
	"This method calls the the platform specific unset environment routine"

	^ self ffiCall: #(int unsetenv #(String string)) module: LibC
]
