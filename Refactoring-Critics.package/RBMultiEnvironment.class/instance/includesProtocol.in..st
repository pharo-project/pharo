testing
includesProtocol: aProtocol in: aClass
	^ (super includesProtocol: aProtocol in: aClass) and: [ environmentDictionaries anySatisfy: [ :env | env includesProtocol: aProtocol in: aClass ] ]