classification-rules
classifyByKnownPragma: aMethod
	pragmaMapping keysAndValuesDo: [ :pragma :protocol |
		(aMethod hasPragmaNamed: pragma)
			ifTrue: [
				aMethod protocol: protocol.
				^ true ]].
	^ false.