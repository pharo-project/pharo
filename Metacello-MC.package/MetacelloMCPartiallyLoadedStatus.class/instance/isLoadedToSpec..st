testing
isLoadedToSpec: matchBlock
	"all loaded projects and packages match specifications (at least one package loaded)"

	| matchStatus |
	matchStatus := #loadedToSpec.
	(self evaluateStatus: #(#allLoadedToSpec #loadedToSpec))
		ifTrue: [ 
			aPackageIsLoaded & aProjectIsLoaded
				ifTrue: [ 
					(aLoadedPackageIsExact & aLoadedPackageIsCurrent not & aLoadedPackageIsNotCurrent not
						and: [ aLoadedProjectIsExact & aLoadedProjectIsCurrent not & aLoadedProjectIsNotCurrent not ])
						ifTrue: [ matchBlock value: matchStatus ] ]
				ifFalse: [ 
					aPackageIsLoaded
						ifTrue: [ 
							aLoadedPackageIsExact & aLoadedPackageIsCurrent not & aLoadedPackageIsNotCurrent not
								ifTrue: [ matchBlock value: matchStatus ] ]
						ifFalse: [ 
							hasNoPackage & aLoadedProjectIsExact & aLoadedProjectIsCurrent not & aLoadedProjectIsNotCurrent not
								ifTrue: [ matchBlock value: matchStatus ] ] ] ]