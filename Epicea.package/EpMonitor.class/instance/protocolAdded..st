announcement handling
protocolAdded: aProtocolAdded 
	self addEvent: 
		(EpProtocolAddition 
			behavior: aProtocolAdded classAffected
			protocol: aProtocolAdded protocol)