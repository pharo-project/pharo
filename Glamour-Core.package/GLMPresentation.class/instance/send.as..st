scripting
send: aBlock as: aPortSymbol
	(self
			rawSelectionTransmissionFor: aPortSymbol
			ifNone: [ self addRawSelectionTransmissionTo: aPortSymbol ])
		transformation: aBlock