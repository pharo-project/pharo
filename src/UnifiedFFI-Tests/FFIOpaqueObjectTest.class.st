"
Tests for FFIOpaqueObjectType
"
Class {
	#name : #FFIOpaqueObjectTest,
	#superclass : #TestCase,
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #private }
FFIOpaqueObjectTest >> ffiBindingOf: aName [
	^ self class ffiBindingOf: aName
]

{ #category : #private }
FFIOpaqueObjectTest >> ffiInstVarArgument: argName generator: generator [
	^ argName
]

{ #category : #tests }
FFIOpaqueObjectTest >> newParser [ 
	^ FFIFunctionParser new
		requestor: (FFICallout new
			requestor: self;
			yourself);
		yourself
]

{ #category : #tests }
FFIOpaqueObjectTest >> testDereferencedExternalObjectError [
	self 
		should: [ self newParser parseNamedFunction: #( void function ( FFIOpaqueObject arg ) ) ]
		raise: FFIDereferencedOpaqueObjectError.
	self 
		should: [ self newParser parseNamedFunction: #( FFIOpaqueObject function ( ) ) ]
		raise: FFIDereferencedOpaqueObjectError
		

]

{ #category : #tests }
FFIOpaqueObjectTest >> testParseAsBigArityPointer [
	 | fnSpec arg returnType |
	
	fnSpec := self newParser parseNamedFunction: #( void function ( FFIOpaqueObject **arg ) ). 
	arg := fnSpec arguments first.
	self assert: arg class equals: FFIOpaqueObjectType.
	self assert: arg pointerArity equals: 2.
	
	fnSpec := self newParser parseNamedFunction: #( FFIOpaqueObject **function ( ) ). 
	returnType := fnSpec returnType.
	self assert: returnType class equals: FFIOpaqueObjectType.
	self assert: returnType pointerArity equals: 2
]

{ #category : #tests }
FFIOpaqueObjectTest >> testParseAsPointer [
	 | fnSpec arg returnType |
	
	fnSpec := self newParser parseNamedFunction: #( void function ( FFIOpaqueObject *arg ) ). 
	arg := fnSpec arguments first.
	self assert: arg class equals: FFIOpaqueObjectType.
	self assert: arg pointerArity equals: 1.
	
	fnSpec := self newParser parseNamedFunction: #( FFIOpaqueObject *function ( ) ). 
	returnType := fnSpec returnType.
	self assert: returnType class equals: FFIOpaqueObjectType.
	self assert: returnType pointerArity equals: 1
]
