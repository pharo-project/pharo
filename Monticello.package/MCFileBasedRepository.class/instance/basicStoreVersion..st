storing
basicStoreVersion: aVersion
	self
		writeStreamForFileNamed: aVersion fileName
		do: [:s | aVersion fileOutOn: s].
	aVersion isCacheable ifTrue: [
		cache ifNil: [cache := Dictionary new].
		cache at: aVersion fileName put: aVersion].
