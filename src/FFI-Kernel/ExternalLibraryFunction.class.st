"
An ExternalLibraryFunction specifies a fully qualified function from an external library.

Instance variables:
	name			<String | Integer>	name or ordinal of function
	module			<String | nil>		name of module (nil if bound in the VM).
	errorCodeName <String | nil>		name of temp receiving error code, if any
"
Class {
	#name : #ExternalLibraryFunction,
	#superclass : #ExternalFunction,
	#instVars : [
		'name',
		'module',
		'errorCodeName'
	],
	#category : #'FFI-Kernel'
}

{ #category : #'instance creation' }
ExternalLibraryFunction class >> name: aName module: aModule callType: callType returnType: retType argumentTypes: argTypes [
	^self new
		name: aName
		module: aModule
		flags: callType
		argTypes: (Array with: retType), argTypes
]

{ #category : #comparing }
ExternalLibraryFunction >> analogousCodeTo: anObject [
	^(anObject isKindOf: ExternalLibraryFunction)
	and: [flags = anObject flags
	and: [argTypes = anObject argTypes
	and: [name = anObject name
	and: [module = anObject module
	and: [errorCodeName = anObject errorCodeName]]]]]
]

{ #category : #accessing }
ExternalLibraryFunction >> errorCodeName [
	^errorCodeName
]

{ #category : #accessing }
ExternalLibraryFunction >> module [
	^module
]

{ #category : #accessing }
ExternalLibraryFunction >> name [
	^name
]

{ #category : #private }
ExternalLibraryFunction >> name: aName module: aModule flags: anInteger argTypes: argTypeArray [

	name := aName.
	module := aModule.
	flags := anInteger.
	argTypes := argTypeArray.
]

{ #category : #accessing }
ExternalLibraryFunction >> setErrorCodeName: aString [
	errorCodeName := aString
]

{ #category : #accessing }
ExternalLibraryFunction >> setModule: aString [
	"Private. Hack the module"
	module := aString.
]
