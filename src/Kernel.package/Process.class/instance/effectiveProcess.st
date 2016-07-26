accessing
effectiveProcess
	"effectiveProcess is a mechanism to allow process-faithful debugging.  The debugger executes code
	 on behalf of processes, so unless some effort is made the identity of Processor activeProcess is not
	 correctly maintained when debugging code.  The debugger uses evaluate:onBehalfOf: to assign the
	 debugged process as the effectiveProcess of the process executing the code, preserving process
	 identity."
	^effectiveProcess ifNil: [self] 