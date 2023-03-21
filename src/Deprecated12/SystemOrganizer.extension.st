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

{ #category : #'*Deprecated12' }
SystemOrganizer >> categoriesMatching: matchString [

	self deprecated: 'Use #packageNamesMatching: instead' transformWith: '`@rcv categoriesMatching: `@arg' -> '`@rcv packageNamesMatching: `@arg'.
	^ self packageNamesMatching: matchString
]

{ #category : #'*Deprecated12' }
SystemOrganizer >> categoryOfElement: element [

	self deprecated: 'Use #packageNameOfElement: instead' transformWith: '`@rcv categoryOfElement: `@arg' -> '`@rcv packageNameOfElement: `@arg'.
	^ self packageNameOfElement: element
]

{ #category : #'*Deprecated12' }
SystemOrganizer >> classesInCategory: packageName [

	self deprecated: 'Use #classesInPackageNamed: instead' transformWith: '`@rcv classesInCategory: `@arg' -> '`@rcv classesInPackageNamed: `@arg'.
	^ self classesInPackageNamed: packageName
]

{ #category : #'*Deprecated12' }
SystemOrganizer >> includesCategory: packageName [

	self deprecated: 'Use #includesPackageNamed: instead' transformWith: '`@rcv includesCategory: `@arg' -> '`@rcv includesPackageNamed: `@arg'.
	^ self includesPackageNamed: packageName
]

{ #category : #'*Deprecated12' }
SystemOrganizer >> listAtCategoryNamed: packageName [

	self deprecated: 'Use #classNamesInPackageNamed: instead' transformWith: '`@rcv listAtCategoryNamed: `@arg' -> '`@rcv classNamesInPackageNamed: `@arg'.
	^ self classNamesInPackageNamed: packageName
]

{ #category : #'*Deprecated12' }
SystemOrganizer >> removeCategoriesMatching: matchString [

	self deprecated: 'Use #removePackageNamesMatching: instead' transformWith: '`@rcv removeCategoriesMatching: `@arg' -> '`@rcv removePackageNamesMatching: `@arg'.
	^ self removePackageNamesMatching: matchString
]

{ #category : #'*Deprecated12' }
SystemOrganizer >> removeCategory: packageName [

	self deprecated: 'Use #removePackageNamed: instead' transformWith: '`@rcv removeCategory: `@arg' -> '`@rcv removePackageNamed: `@arg'.
	^ self removePackageNamed: packageName
]

{ #category : #'*Deprecated12' }
SystemOrganizer >> removeSystemCategory: packageName [

	self deprecated: 'Use #removeFromSystemPackageName: instead' transformWith: '`@rcv removeSystemCategory: `@arg' -> '`@rcv removeFromSystemPackageName: `@arg'.
	^ self removeFromSystemPackageName: packageName
]

{ #category : #'*Deprecated12' }
SystemOrganizer >> renameCategory: oldPackageName toBe: newPackageName [

	self
		deprecated: 'Use #renamePackageNamed:to: instead'
		transformWith: '`@rcv renameCategory: `@arg toBe: `@arg2' -> '`@rcv renamePackageNamed: `@arg to: `@arg2'.
	^ self renamePackageNamed: oldPackageName to: newPackageName
]
