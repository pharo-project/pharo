private
stepToFirstInterestingBytecodeIn: aProcess
	"Do not step over any bytecode."
	
	^ aProcess suspendedContext