classification-rules
classifyByKnownPrefix: aMethod
	(self protocolForKnownPrefixOfSelector: aMethod selector)
		ifNil: [ ^ false ]
		ifNotNil: [ :protocol | 
			aMethod protocol: protocol.
			^ true ]