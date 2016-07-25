events
mouseEnter: evt
	"The mouse entered the receiver"

	owner ifNotNil: [owner stayUp ifFalse: [self mouseEnterDragging: evt]].