classification-rules
classifyByKnownPragma: aMethod
	self class pragmaMapping
		keysAndValuesDo: [ :pragma :protocol | 
			(aMethod hasPragmaNamed: pragma)
				ifTrue: [ aMethod protocol: protocol.
					^ true ] ].
	^ false