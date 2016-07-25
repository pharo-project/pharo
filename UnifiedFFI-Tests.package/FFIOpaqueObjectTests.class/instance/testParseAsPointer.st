tests
testParseAsPointer
	 | fnSpec arg returnType |
	
	fnSpec := self newParser parseNamedFunction: #( void function ( FFIOpaqueObject *arg ) ). 
	arg := fnSpec arguments first.
	self assert: arg class equals: FFIOpaqueObjectType.
	self assert: arg pointerArity equals: 1.
	
	fnSpec := self newParser parseNamedFunction: #( FFIOpaqueObject *function ( ) ). 
	returnType := fnSpec returnType.
	self assert: returnType class equals: FFIOpaqueObjectType.
	self assert: returnType pointerArity equals: 1.