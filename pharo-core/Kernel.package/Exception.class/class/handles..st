exceptionselector
handles: exception
	"Determine whether an exception handler will accept a signaled exception."

	^ exception isKindOf: self