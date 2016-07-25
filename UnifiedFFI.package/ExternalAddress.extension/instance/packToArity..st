*UnifiedFFI-private
packToArity: arity
	"This will 'pack' a pointer to a certain arity. 
	What does this means? Let's say I have a method who will be called with a ** arity (that's 'arity 2'): 
	
	method: aPointer
		self ffiCall: #( method ( MyExternalObjectClass **aPointer) )
		
	This usually means that method will put a pointer in the address of aPointer. 
	And what I actually want is this pointer. So I do Something like this: 
	
	p := MyExternalObjectClass new.
	self mehod: p.
	
	And I expect to have 'p' correctly initialised on return. 
	Well... tha's not so simple :)
	
	When compiling #method:, UnifiedFFI needs to first 'pack' the pointer (which means to 
	take a pointer of a pointer of a pointer... as many pointers as arity solicited), and then, 
	after call, it needs to 'unpack' all this pointers (see #unpackFromArity: method)  
	"
	| rolledPointer |
		
	rolledPointer := self.
	"Start in 2 because first pointer is self"
	2 to: arity do: [ :index | rolledPointer := rolledPointer pointerAutoRelease ].
	^ rolledPointer