events
mouseEnter: evt
	"The mouse entered the receiver"

	owner ifNotNil: [owner stayUp ifFalse: [self mouseEnterDragging: evt]].
	(owner selectedItem notNil
				and: [owner selectedItem ~~ self])
			ifTrue: [owner selectItem: self event: evt.].

