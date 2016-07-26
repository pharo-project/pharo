A FullBlockClosure is a closure that can be independent of any outerContext if desired.  It has its own method (currently reusing the startpc inst var) and its own receiver.  outerContext can be either a MethodContext/Context or nil.

Instance Variables
	outerContext 	<Context>
	(startpc) compiledBlock <CompiledBlock> for compatibility, this is startpc.
	numArgs 		<SmallInteger>
	receiver:		<Object>