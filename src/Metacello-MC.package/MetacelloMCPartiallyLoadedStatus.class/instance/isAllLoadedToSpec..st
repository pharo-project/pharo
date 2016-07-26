testing
isAllLoadedToSpec: matchBlock
        "all projects and packages are loaded and match specification"

	(self evaluateStatus: #(#allLoadedToSpec))
		ifTrue: [ 
			((hasNoPackage
				or: [ 
					aPackageIsLoaded & aPackageNotLoaded not & aLoadedPackageIsExact & aLoadedPackageIsNotCurrent not
						& aLoadedPackageIsCurrent not ])
				and: [ 
					hasNoProject
						or: [ 
							aProjectIsLoaded & aProjectNotLoaded not & aLoadedProjectIsExact & aLoadedProjectIsNotCurrent not
								& aLoadedProjectIsCurrent not ] ])
				ifTrue: [ matchBlock value: #allLoadedToSpec ] ]