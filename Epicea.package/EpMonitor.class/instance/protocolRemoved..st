announcement handling
protocolRemoved: aProtocolRemoved 
	self addEvent: 
		(EpProtocolRemoval
			behavior: aProtocolRemoved classAffected
			protocol: aProtocolRemoved protocol)