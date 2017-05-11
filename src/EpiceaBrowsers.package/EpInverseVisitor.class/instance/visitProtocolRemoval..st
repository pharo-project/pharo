visitor
visitProtocolRemoval: aChange

	^ EpProtocolAddition behavior: aChange behaviorAffected protocol: aChange protocol
