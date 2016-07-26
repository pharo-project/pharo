parsing
extractDefinitionsFrom: member
	| reader |
	(MCSnapshotReader readerClassForFileNamed: member fileName)
		ifNotNil: [:rc |
			reader := rc on: (self contentsForMember: member) readStream.
			definitions addAll: reader definitions]
