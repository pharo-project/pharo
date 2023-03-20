Extension { #name : #SystemOrganizer }

{ #category : #'*Deprecated12' }
SystemOrganizer >> addCategory: packageName [

	self deprecated: 'Use #addPackageNamed: instead' transformWith: '`@rcv addCategory: `@arg' -> '`@rcv addPackageNamed: `@arg'.
	^ self addPackageNamed: packageName
]
