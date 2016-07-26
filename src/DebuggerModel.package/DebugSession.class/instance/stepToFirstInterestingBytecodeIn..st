private
stepToFirstInterestingBytecodeIn: aProcess
	"After a restart of a method activation step to the first 
	bytecode instruction that is of interest for the debugger.
	
	In this case step until a bytecode that causes a context switch,
	as otherwise one will have to press may time step into without 
	seeing any visible results."
	
	^ aProcess stepToSendOrReturn