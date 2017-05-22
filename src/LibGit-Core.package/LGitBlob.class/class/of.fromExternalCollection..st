instance creation
of: aRepository fromExternalCollection: aCollection
	| id |
	self assert: (aCollection class = ByteArray or: [ aCollection class theMetaClass canUnderstand: #asExternalTypeOn: ]).
	id := LGitId new.
	self
		blob_create_frombuffer: id
		repo: aRepository
		buffer: aCollection
		len: aCollection size.
	^ (self of: aRepository)
		initializeWithId: id;
		yourself