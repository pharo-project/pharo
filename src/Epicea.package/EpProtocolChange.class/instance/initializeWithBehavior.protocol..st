initialization-release
initializeWithBehavior: aBehavior protocol: aProtocol
	self initialize.
	behavior := aBehavior asEpiceaRingDefinition.
	protocol := aProtocol asSymbol.