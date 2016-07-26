transmitting
rawSelectionTransmissionFor: aPortSymbol ifNone: ifNoneBlock
	^ rawSelectionTransmissions
		detect: [ :transmission | transmission destination port name = aPortSymbol ]
		ifNone: ifNoneBlock