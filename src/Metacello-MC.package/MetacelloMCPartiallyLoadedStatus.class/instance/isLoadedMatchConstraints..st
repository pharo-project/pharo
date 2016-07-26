testing
isLoadedMatchConstraints: matchBlock
	"all loaded projects and packages match constraints (at least one package loaded)"

	| matchStatus |
	matchStatus := #loadedMatchConstraints.
	(self evaluateStatus: #(#allLoadedToSpec #loadedToSpec #loadedMatchConstraints))
		ifTrue: [ 
			aPackageIsLoaded & aProjectIsLoaded
				ifTrue: [ 
					(aLoadedPackageIsExact | aLoadedPackageIsCurrent & aLoadedPackageIsNotCurrent not
						and: [ aLoadedProjectIsExact | aLoadedProjectIsCurrent & aLoadedProjectIsNotCurrent not ])
						ifTrue: [ matchBlock value: matchStatus ] ]
				ifFalse: [ 
					aPackageIsLoaded
						ifTrue: [ 
							aLoadedPackageIsExact | aLoadedPackageIsCurrent & aLoadedPackageIsNotCurrent not
								ifTrue: [ matchBlock value: matchStatus ] ]
						ifFalse: [ 
							hasNoPackage & (aLoadedProjectIsExact | aLoadedProjectIsCurrent) & aLoadedProjectIsNotCurrent not
								ifTrue: [ matchBlock value: matchStatus ] ] ] ]