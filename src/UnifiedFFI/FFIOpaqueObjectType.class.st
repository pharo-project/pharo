"
I'm  an opaque object type (see ==FFIOpaqueObject== comment)
"
Class {
	#name : #FFIOpaqueObjectType,
	#superclass : #FFIExternalReferenceType,
	#category : #'UnifiedFFI-Types'
}

{ #category : #private }
FFIOpaqueObjectType class >> naturalPointerArity [
	^ 0
]

{ #category : #accessing }
FFIOpaqueObjectType >> instanceVariableName [
	^ #handle
]

{ #category : #testing }
FFIOpaqueObjectType >> needsArityPacking [
	">1 because first is this object, who is a reference (even if not a pointer, is 
	 represented like that because he lives in the C heap. So, we 'roll' if it is 
	 a 'type**' or bigger"
	^ self pointerArity > 1
]

{ #category : #initialization }
FFIOpaqueObjectType >> prepareAsSelfFromCalloutDeclaration [
	"self starts with pointer arity 1 (regardless how is declared later)"
	self pointerArity: 1
]

{ #category : #testing }
FFIOpaqueObjectType >> validateAfterParse: typeAndArityTuple [
	"external objects have a natural arity of zero but they MUST be called with some arity, 
	 because they are actually external addresses (pointers). 
	 That means, you need to always declare external objects as this example:
	 [[[
		self ffiCall: #( FFIExternalObject *c_function ( FFIExternalObject *handle ) )  
	 ]]] 
	"
	self pointerArity >= 1 ifTrue: [ ^ self ].
	typeAndArityTuple second >= 1 ifTrue: [ ^ self ].
	
	FFIDereferencedOpaqueObjectError signal
]
