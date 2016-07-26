status
exitStatusInterpreter
	"#exitStatus answers the bit field integer representing the exit status exactly as returned from #queryExitStatus (waitpid()).
	However, sometimes you want to decode the internal meaning of the integer to get more information about the status. 
	For those purposes, you can use this method and then query the result. 
	For more details, read class comment of OSSUnixProcessExitStatus "
	^ exitStatus ifNotNil: [ OSSUnixProcessExitStatus for: exitStatus ]