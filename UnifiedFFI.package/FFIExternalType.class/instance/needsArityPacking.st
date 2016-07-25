testing
needsArityPacking
	"Regular types needs to be ''rolled'' if they are passed as pointers to its calling functions. 
	 For example, executing consecutivelly this (simplified) two functions: 
	 [[[
	 	time := self ffiCall: #(time_t time(time_t* t) ). ""this will answer a long.""
	 	self ffiCall: #(tm* localtime(time_t* time) ) ""this requires a pointer to time."" 
	 ]]]
	 This mechanism allows UnifiedFFI to perform the roll of this pointers for you (it performs 
	 the equivallent of ==&time== in C). 
	
	 For packing/unpacking logic, arity needs to be bigger than inherent type arity. 
	 Means that if I have a type that is naturally a pointer (for example an ExternalAddress, who 
	 is a 'void*'), it will have a natural arity of 1, then I pack if arity is bigger. 
	 Other cases could need to be rolled when pointer arity is diffrent."
	^ self pointerArity > self class naturalPointerArity