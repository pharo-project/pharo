computing
packagesProvidingSelector: aSelector
	^ aSelector implementors 
		collect: [:each | each package name]
		as: Set