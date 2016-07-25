cleanup
flushEvents
	"EventManager flushEvents"
	
	self actionMaps keysAndValuesDo:[:rcvr :evtDict| rcvr ifNotNil:[
		"make sure we don't modify evtDict while enumerating"
		evtDict keysDo: [:evtName| | msgSet |
			msgSet := evtDict at: evtName ifAbsent:[nil].
			(msgSet == nil) ifTrue:[rcvr removeActionsForEvent: evtName]]]].
	EventManager actionMaps finalizeValues. 