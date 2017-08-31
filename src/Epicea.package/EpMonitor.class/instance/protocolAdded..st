announcement handling
protocolAdded: aProtocolAdded

	self handleAnyErrorDuring: [
		self addEvent: 
			(EpProtocolAddition 
				behavior: aProtocolAdded classAffected
				protocol: aProtocolAdded protocol)]