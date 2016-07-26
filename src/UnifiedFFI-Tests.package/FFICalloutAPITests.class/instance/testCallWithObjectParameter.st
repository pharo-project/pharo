tests object
testCallWithObjectParameter
	"Test a call of the form: 
	
		self nbCall: #( void function ( self ) )
		
		(passing self as parameter)
	"
	| object text |
	
	text := 'Hello, World'.
	object := FFICalloutObjectForTest new
		handle: (ByteArray new: text size + 1);
		yourself. 
	
	self 
		shouldnt: [ FFICalloutObjectForTest primObject: object strcpy: text ] 
		raise: Error.
	self assert: object handle asString = (text copyWith: (Character value: 0))