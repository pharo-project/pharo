Instance variables:
	receiver: <Object> (self)
	closureOrNil: <BlockClosure|nil> 
		nil if I'm a method context
		the blockClosure being executed if I'm a block context
	method <CompiledMethod> 
		method being executed if I'm a method context
		method holding the block if I'm a block context
	variable fields: <Object> temporary variables (including arguments)

My instances hold all the dynamic state associated with the execution of either a method activation resulting from a message send or a block activation resulting from a block evaluation.
	
MethodContexts, though normal in their variable size, are actually only used in two sizes, small and large, which are determined by the temporary space required by the method being executed.

MethodContexts must only be created using the method newForMethod:.  Note that it is impossible to determine the real object size of a MethodContext except by asking for the frameSize of its method.  Any fields above the stack pointer (stackp) are truly invisible -- even (and especially!) to the garbage collector.  Any store into stackp other than by the primitive method stackp: is potentially fatal.