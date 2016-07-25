tests object
testCallWithObjectCreation
	"Test a call of the form: 
	
		self nbCall: #( NBFFICalloutVMObjectForTest malloc ( size_t size ) )
		
		(obtaining an object as result)
	"
	| object |
	
	[
		self shouldnt: [ object := FFICalloutObjectForTest primCreate: 1 ] raise: Error.
		self assert: object notNil.
		self assert: object class = FFICalloutObjectForTest.
		self assert: object handle notNil.
		self deny: object handle isNull ]
	ensure: [ 
		object ifNotNil: [ object free ] ]