Extension { #name : #String }

{ #category : #'*UnifiedFFI' }
String >> asExternalTypeOn: generator [
	^ generator resolveType: self
]

{ #category : #'*UnifiedFFI' }
String class >> asExternalTypeOn: generator [ 
	^ generator resolveType: #FFIExternalString
]

{ #category : #'*UnifiedFFI' }
String >> asFFILibrary [ 
	^ FFIUnknownLibrary name: self
]

{ #category : #'*UnifiedFFI' }
String >> packToArity: arity [
	"WARNING: Here applies the same rules as in ByteArray>>#rollToArity:, check there!"
	arity = 1 ifTrue: [ ^ self ].
	self error: 'Use ExternalAddress instead!'
]
