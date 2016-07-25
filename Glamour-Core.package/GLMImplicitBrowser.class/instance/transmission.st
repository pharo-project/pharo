accessing
transmission
	^ transmission ifNil: [
		transmission := GLMTransmission new ensureReplacePresentationsStrategy.
		transmission ]