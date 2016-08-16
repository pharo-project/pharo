classification-rules
classifyByKnownKeywordSuffix: aMethod
	(self protocolForKnownKeywordSuffixOfSelector: aMethod selector)
		ifNil: [ ^ false ]
		ifNotNil: [ :protocol | 
			aMethod protocol: protocol.
			^ true ]