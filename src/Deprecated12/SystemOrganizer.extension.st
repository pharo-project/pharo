Extension { #name : #SystemOrganizer }

{ #category : #'*Deprecated12' }
SystemOrganizer >> addCategory: packageName [

	self deprecated: 'Use #addPackageNamed: instead' transformWith: '`@rcv addCategory: `@arg' -> '`@rcv addPackageNamed: `@arg'.
	^ self addPackageNamed: packageName
]

{ #category : #'*Deprecated12' }
SystemOrganizer >> categories [

	self deprecated: 'Use #packageNames instead' transformWith: '`@rcv categories' -> '`@rcv packageNames'.
	^ self packageNames
]
