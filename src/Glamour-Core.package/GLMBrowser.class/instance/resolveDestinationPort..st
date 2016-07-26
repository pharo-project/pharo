scripting-private
resolveDestinationPort: aPortReference 
	self flag: 'this is a temporary hack due to initializeScripting transmitting directly a port'.
	(aPortReference isKindOf: GLMPort) ifTrue: [ ^ aPortReference ].
	^self resolvePort: aPortReference asGlamourTargetIdentifier defaultPortName: #entity