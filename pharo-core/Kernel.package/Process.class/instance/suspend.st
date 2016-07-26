changing process state
suspend
	"Primitive. Stop the process that the receiver represents in such a way 
	that it can be restarted at a later time (by sending the receiver the 
	message resume). If the receiver represents the activeProcess, suspend it. 
	Otherwise remove the receiver from the list of waiting processes.
	The return value of this method is the list the receiver was previously on (if any)."
	| oldList |
	<primitive: 88>
	"This is fallback code for VMs which only support the old primitiveSuspend which 
	would not accept processes that are waiting to be run."
	myList ifNil:[^nil]. "this allows us to use suspend multiple times"
	oldList := myList.
	myList := nil.
	oldList remove: self ifAbsent:[].
	^oldList