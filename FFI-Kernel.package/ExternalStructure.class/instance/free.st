initialize-release
free
	"Free the handle pointed to by the receiver"
	(handle ~~ nil and:[handle isExternalAddress]) ifTrue:[handle free].
	handle := nil.