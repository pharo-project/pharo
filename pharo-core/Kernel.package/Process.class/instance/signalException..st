signaling
signalException: anException
	"Signal an exception in the receiver process...if the receiver is currently
	suspended, the exception will get signaled when the receiver is resumed.  If 
	the receiver is blocked on a Semaphore, it will be immediately re-awakened
	and the exception will be signaled; if the exception is resumed, then the receiver
	will return to a blocked state unless the blocking Semaphore has excess signals"
	| oldList |
	"If we are the active process, go ahead and signal the exception"
	self isActiveProcess ifTrue: [^anException signal].

	"Suspend myself first to ensure that I won't run away in the
	midst of the following modifications."
	myList ifNotNil:[oldList := self suspend].

	"Add a new method context to the stack that will signal the exception"
	suspendedContext := Context
		sender: suspendedContext
		receiver: self
		method: (self class lookupSelector: #pvtSignal:list:)
		arguments: (Array with: anException with: oldList).

	"If we are on a list to run, then suspend and restart the receiver 
	(this lets the receiver run if it is currently blocked on a semaphore).  If
	we are not on a list to be run (i.e. this process is suspended), then when the
	process is resumed, it will signal the exception"

	oldList ifNotNil: [self resume].
