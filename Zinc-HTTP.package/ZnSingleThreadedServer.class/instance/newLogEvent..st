private logging
newLogEvent: logEventClass
	^ logEventClass new
		serverId: self route;
		yourself