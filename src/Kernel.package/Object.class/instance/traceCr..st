tracing
traceCr: aString
	"Log the argument. Use self traceCr: 'something' instead of Transcript show: 'something'  ; cr "
	
	Transcript show: aString ; cr