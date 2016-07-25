*UnifiedFFI
packToArity: arity
	"This is complicated... I assuming this ways of calling a function: 
	 
   arity == 1: 
	-----------
	ByteArray works as pointer buffer of single pointer to something: 
	ex 1)
		buffer := ByteArray new: 4.
		self ffiCall: #( void function (int *buffer) ).
	ex 2)
		buffer := 'Hello, World' asByteArray.
		self ffiCall: #( void function (char *buffer) ).
	
	arity > 1: 
	----------
	ByteArray works as pointer to allocate results:
	ex 1)
		pointer := ByteArray new: (FFIExternalType pointerSize).
		self ffiCall: #( void function ( char **answer )).
	   
	In this case this will not work fine because content of ByteArray needs to be a 
	pointer too, and then it needs to be allocated in the heap... while this could be 
	managed, I'm puting for the moment just an error and a recommendation of using an 
	ExternalAddress.
	"
	arity = 1 ifTrue: [ ^ self ].
	self error: 'Use ExternalAddress instead!'