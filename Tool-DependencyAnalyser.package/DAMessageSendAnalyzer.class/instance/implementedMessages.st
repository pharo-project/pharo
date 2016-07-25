computing
implementedMessages
	^ (self packageStaticDependencies 
		add: self packageName;
		yourself)
		flatCollect: [ :name | (RPackageOrganizer default packageNamed: name) selectors ]