system changes
classRemovedEventOccurs: anEvent
	"a class has been removed: first see if the class is not my class
	because then I must be unplugged from system event notifications"
	anEvent classRemoved = self class
		ifTrue: [^ self noMoreNotifications].
	"remove all handler which are from the removed class"
	(self pragmasOfClass: anEvent classRemoved class)
		do: [:handler | self removePragma: handler] 