actions
addRepositoryToPackage
	| selectedPackages |
	self repository
		ifNotNil: [ :repos | 
			((SearchFacade mcPackageSearchRejectAll: [ :aPackage | aPackage repositoryGroup includes: repos ] withInitialFilter: nil)
				chooseFromOwner: self window)
				ifNotNil: [ :wc | 
					workingCopy := wc.
					workingCopy repositoryGroup addRepository: repos.
					self repository: repos.
					self
						changed: #workingCopySelection;
						repositoryListChanged;
						changed: #repositorySelection.
					self changedButtons ] ]