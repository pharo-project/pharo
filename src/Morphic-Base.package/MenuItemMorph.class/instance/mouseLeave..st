events
mouseLeave: evt
	"The mouse has left the interior of the receiver..."

	owner ifNotNil: [owner stayUp ifFalse: [self mouseLeaveDragging: evt]]