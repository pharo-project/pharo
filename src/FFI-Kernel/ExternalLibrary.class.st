"
An external library bundles calls to functions from the same library. It is provided mainly as convenience since every external function can be fully specified by the name and the module it resides in.

Every external function that is defined in an external library by default will use the library it is defined in. This can always be modified by providing the appropriate module in the specification. 
"
Class {
	#name : #ExternalLibrary,
	#superclass : #ExternalObject,
	#instVars : [
		'name'
	],
	#category : #'FFI-Kernel'
}

{ #category : #accessing }
ExternalLibrary class >> moduleName [
	"Return the name of the module for this library"
	^nil
]

{ #category : #initialization }
ExternalLibrary >> forceLoading [
	"Primitive. Force loading the given library.
	The primitive will fail if the library is not available
	or if anything is wrong with the receiver."
	<primitive: 'primitiveForceLoad' module:'SqueakFFIPrims'>
	^self externalCallFailed "The primitive will set the error code"
]

{ #category : #accessing }
ExternalLibrary >> handle [
	^handle
]

{ #category : #initialization }
ExternalLibrary >> initialize [
	"Initialize the receiver"
	name := self class moduleName.
	handle := ExternalAddress new.
]

{ #category : #accessing }
ExternalLibrary >> name [
	^name
]
