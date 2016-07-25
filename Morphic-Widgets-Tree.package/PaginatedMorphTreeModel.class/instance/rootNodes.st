accessing
rootNodes
	| computedRootNodes |
	computedRootNodes := super rootNodes.
	self defaultChunkSize 
		ifNotNil: [computedRootNodes size > self defaultChunkSize
			ifTrue: [self chunkSize: self defaultChunkSize]]
		ifNil: [
			self defaultPageSize 
				ifNotNil: [computedRootNodes size > self defaultPageSize
					ifTrue: [self pageSize: self defaultPageSize]]].
	^ computedRootNodes
