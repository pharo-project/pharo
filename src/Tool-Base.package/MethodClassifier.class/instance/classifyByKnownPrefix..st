classification-rules
classifyByKnownPrefix: aMethod
	prefixMapping keysAndValuesDo: [ :prefix :protocol |
		(aMethod selector beginsWith: prefix)
			ifTrue: [
				aMethod protocol: protocol.
				^ true ]].
	^ false.