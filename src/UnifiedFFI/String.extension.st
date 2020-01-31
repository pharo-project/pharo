Extension { #name : #String }

{ #category : #'*UnifiedFFI' }
String class >> asExternalTypeOn: generator [ 
	^ generator resolveType: #FFIExternalString
]

{ #category : #'*UnifiedFFI' }
String >> asExternalTypeOn: generator [
	^ generator resolveType: self
]

{ #category : #'*UnifiedFFI' }
String >> asFFILibrary [ 
	^ FFIUnknownLibrary name: self
]

{ #category : #'*UnifiedFFI' }
String >> nullTerminatedEncodeWith: encoding [
	"Produce a ByteArray that encodes the receiver, using a specified encoding and adding a null terminator.
	Encoding is either a ZnCharacterEncoder instance or an identifier for one.
	Useful extension for FFI and C integration"
	
	" 'Les élèves français' nullTerminatedEncodeWith: #utf8 "
	
	^ ByteArray streamContents: [ :stream |
		encoding asZnCharacterEncoder next: self size putAll: self startingAt: 1 toStream: stream.
		stream nextPut: 0 "Null terminator" ]
]

{ #category : #'*UnifiedFFI' }
String >> packToArity: arity [
	"WARNING: Here applies the same rules as in ByteArray>>#rollToArity:, check there!"
	arity = 1 ifTrue: [ ^ self ].
	self error: 'Use ExternalAddress instead!'
]
