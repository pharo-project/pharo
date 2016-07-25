updating
updateList
	self isRendered ifTrue: [
		self treeHolder updateList.
		self changed: #getDescription.
		self changed: #getViewedPackageNames.
		super updateList].	
