accessing method dictionary
addSelector: selector withMethod: compiledMethod notifying: requestor
	| priorMethodOrNil oldProtocol |
	priorMethodOrNil := self compiledMethodAt: selector ifAbsent: [nil].
	oldProtocol := priorMethodOrNil ifNotNil: [ priorMethodOrNil protocol ].
	self addSelectorSilently: selector withMethod: compiledMethod.
	priorMethodOrNil 
		ifNil: [SystemAnnouncer uniqueInstance methodAdded: compiledMethod ]
		ifNotNil: [SystemAnnouncer uniqueInstance methodChangedFrom: priorMethodOrNil to: compiledMethod oldProtocol: oldProtocol ].