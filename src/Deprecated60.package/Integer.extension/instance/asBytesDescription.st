*Deprecated60
asBytesDescription
	self
		deprecated: 'Please use #humanReadableSIByteSize instead'
		transformWith: '`@receiver asBytesDescription' -> '`@receiver humanReadableSIByteSize'.
	^ self humanReadableSIByteSize