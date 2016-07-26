tests
testDereferencedExternalObjectError
	self 
		should: [ self newParser parseNamedFunction: #( void function ( FFIOpaqueObject arg ) ) ]
		raise: FFIDereferencedOpaqueObjectError.
	self 
		should: [ self newParser parseNamedFunction: #( FFIOpaqueObject function ( ) ) ]
		raise: FFIDereferencedOpaqueObjectError
		
