exceptionselector
handles: anException
	"Determine whether an exception handler will accept a signaled exception."

	^ exceptions anySatisfy: [ :exception | exception handles: anException ]