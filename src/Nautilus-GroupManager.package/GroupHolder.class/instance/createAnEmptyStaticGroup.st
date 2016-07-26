adding
createAnEmptyStaticGroup

	| entry |
	entry := UIManager default request: 'Name of the new group:' initialAnswer: '' title: 'Create a new group'.
	^ (entry isNil or: [entry isEmpty])
		ifFalse: [ self addAnEmptyDynamicGroupNamed: entry ]