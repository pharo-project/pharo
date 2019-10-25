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
	| fnSpec |
	self 
		should: [ 
			fnSpec := self newParser parseNamedFunction: #( void function ( FFIOpaqueObject arg ) ).
			fnSpec resolveUsing: self newParser requestor. ]
		raise: FFIDereferencedOpaqueObjectError.
	self 
		should: [ 
			fnSpec := self newParser parseNamedFunction: #( FFIOpaqueObject function ( ) ).
			fnSpec resolveUsing: self newParser requestor. ]
		raise: FFIDereferencedOpaqueObjectError
		

]

{ #category : #tests }
FFIOpaqueObjectTest >> testParseAsBigArityPointer [
	 | fnSpec arg returnType |
	
	fnSpec := self newParser parseNamedFunction: #( void function ( FFIOpaqueObject **arg ) ). 
	fnSpec resolveUsing: self newParser requestor.
	arg := fnSpec arguments first.
	self assert: arg resolvedType class equals: FFIOpaqueObjectType.
	self assert: arg resolvedType pointerArity equals: 2.
	
	fnSpec := self newParser parseNamedFunction: #( FFIOpaqueObject **function ( ) ). 
	fnSpec resolveUsing: self newParser requestor.
	returnType := fnSpec returnType.
	self assert: returnType resolvedType class equals: FFIOpaqueObjectType.
	self assert: returnType resolvedType pointerArity equals: 2
]

{ #category : #tests }
FFIOpaqueObjectTest >> testParseAsPointer [
	 | fnSpec arg returnType |
	
	fnSpec := self newParser parseNamedFunction: #( void function ( FFIOpaqueObject *arg ) ). 
	fnSpec resolveUsing: self newParser requestor.
	arg := fnSpec arguments first.
	self assert: arg resolvedType class equals: FFIOpaqueObjectType.
	self assert: arg resolvedType pointerArity equals: 1.
	
	fnSpec := self newParser parseNamedFunction: #( FFIOpaqueObject *function ( ) ). 
	fnSpec resolveUsing: self newParser requestor.
	returnType := fnSpec returnType.
	self assert: returnType resolvedType class equals: FFIOpaqueObjectType.
	self assert: returnType resolvedType pointerArity equals: 1
]
