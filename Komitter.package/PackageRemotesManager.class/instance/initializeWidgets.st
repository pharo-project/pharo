initialization
initializeWidgets

	packages := self instantiate: SearchableTree.
	remotes := self instantiate: RemotesManager.

	packages displayBlock: [ :each | each packageName ].
	
	remotes selectedRemotes: self selectedPackageRepositories.
		
	self focusOrder
		add: packages;
		add: remotes