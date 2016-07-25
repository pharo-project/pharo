accessing
rootNodes
	^ (self rootItems ifNil: [^nil])
		collect: [:ci | self rootNodeFromItem: ci].
