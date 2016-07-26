versions
currentVersionAgainst: resolvedPackageAndProjectNames
	| cacheKey |
	cacheKey := resolvedPackageAndProjectNames isNil
		ifTrue: [ Array with: self configuration class with: nil ]
		ifFalse: [ Array with: self configuration class with: (resolvedPackageAndProjectNames sort: [ :a :b | a <= b ]) ].
	^ MetacelloPlatform current
		stackCacheFor: #currentVersionAgainst:
		at: cacheKey
		doing: [ :cache | 
			| cv versions latestSomethingLoaded |
			cv := nil.
			versions := self sortedAndFilteredVersions.
			versions
				do: [ :version | 
					| status matchBlock |
					status := resolvedPackageAndProjectNames isNil
						ifTrue: [ version spec isPartiallyCurrent ]
						ifFalse: [ version spec isPartiallyCurrentAgainst: resolvedPackageAndProjectNames ].
					matchBlock := [ :matchStatus | 
					cv := version copy.
					cv versionStatus: matchStatus.
					^ cache at: cacheKey put: cv ].
					status isAllLoadedToSpec: matchBlock.
					status isLoadedToSpec: matchBlock.
					status isLoadedMatchConstraints: matchBlock.
					status
						isSomethingLoaded: [ :matchStatus | 
							latestSomethingLoaded isNil
								ifTrue: [ 
									cv := version copy.
									cv versionStatus: matchStatus.
									latestSomethingLoaded := cv ] ] ].
			latestSomethingLoaded ifNotNil: [ ^ cache at: cacheKey put: latestSomethingLoaded ].
			^ cache at: cacheKey put: nil ]