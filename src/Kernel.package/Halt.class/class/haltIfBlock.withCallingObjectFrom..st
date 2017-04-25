private
haltIfBlock: aBlock withCallingObjectFrom: haltSenderContext

	(aBlock cull: haltSenderContext receiver)
		ifTrue: [self signalIn: haltSenderContext]
		ifFalse: [^ self].