announcement handling
protocolRemoved: aProtocolRemoved 

	"Skip an irrelevant case"
	aProtocolRemoved protocol = Protocol unclassified ifTrue: [ ^self ].

	self handleAnyErrorDuring: [
		self addEvent: 
			(EpProtocolRemoval
				behavior: aProtocolRemoved classAffected
				protocol: aProtocolRemoved protocol)]