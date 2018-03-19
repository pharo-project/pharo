"
Tests for FFIOpaqueObjectType
"
Class {
	#name : #FFIOpaqueObjectTests,
	#superclass : #TestCase,
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #private }
FFIOpaqueObjectTests >> ffiBindingOf: aName [
	^ self class ffiBindingOf: aName
]

{ #category : #private }
FFIOpaqueObjectTests >> ffiInstVarArgument: argName generator: generator [
	^ argName
]

{ #category : #tests }
FFIOpaqueObjectTests >> newParser [ 
	^ FFIFunctionParser new
		requestor: (FFICallout new
			requestor: self;
			yourself);
		yourself
]

{ #category : #tests }
FFIOpaqueObjectTests >> testDereferencedExternalObjectError [
	self 
		should: [ self newParser parseNamedFunction: #( void function ( FFIOpaqueObject arg ) ) ]
		raise: FFIDereferencedOpaqueObjectError.
	self 
		should: [ self newParser parseNamedFunction: #( FFIOpaqueObject function ( ) ) ]
		raise: FFIDereferencedOpaqueObjectError
		

]

{ #category : #tests }
FFIOpaqueObjectTests >> testParseAsBigArityPointer [
	 | fnSpec arg returnType |
	
	fnSpec := self newParser parseNamedFunction: #( void function ( FFIOpaqueObject **arg ) ). 
	arg := fnSpec arguments first.
	self assert: arg class equals: FFIOpaqueObjectType.
	self assert: arg pointerArity equals: 2.
	
	fnSpec := self newParser parseNamedFunction: #( FFIOpaqueObject **function ( ) ). 
	returnType := fnSpec returnType.
	self assert: returnType class equals: FFIOpaqueObjectType.
	self assert: returnType pointerArity equals: 2.
]

{ #category : #tests }
FFIOpaqueObjectTests >> testParseAsPointer [
	 | fnSpec arg returnType |
	
	fnSpec := self newParser parseNamedFunction: #( void function ( FFIOpaqueObject *arg ) ). 
	arg := fnSpec arguments first.
	self assert: arg class equals: FFIOpaqueObjectType.
	self assert: arg pointerArity equals: 1.
	
	fnSpec := self newParser parseNamedFunction: #( FFIOpaqueObject *function ( ) ). 
	returnType := fnSpec returnType.
	self assert: returnType class equals: FFIOpaqueObjectType.
	self assert: returnType pointerArity equals: 1.
]
