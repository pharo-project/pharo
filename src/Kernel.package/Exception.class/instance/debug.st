handling
debug
	"open a debugger on myself"
	Processor activeProcess 
		debug: self signalerContext
		title: self description