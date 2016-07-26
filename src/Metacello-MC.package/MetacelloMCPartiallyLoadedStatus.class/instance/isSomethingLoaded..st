testing
isSomethingLoaded: somethingLoadedBlock
	"at least one package loaded"

	(self evaluateStatus: #(#allLoadedToSpec #loadedToSpec #loadedMatchConstraints #somethingLoaded))
		ifTrue: [ 
			aPackageIsLoaded
				ifTrue: [ somethingLoadedBlock value: #somethingLoaded ] ]