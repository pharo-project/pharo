tests object
testCallWithSelfParameter
	"Test a call of the form: 
	
		self nbCall: #( void function ( self ) )
		
		(passing self as parameter)
	"
	| object |
	
	object := FFICalloutObjectForTest new. 
	self 
		shouldnt: [ object strcpy: 'Hello, World' ] 
		raise: Error.
	"Since is a strcpy it will answer a byte array terminated in zero"
	self assert: object handle asString = ('Hello, World' copyWith: (Character value: 0))