querying
versions
	^ self repository allVersionDescriptors 
		select: [ :each | each packageName = self package packageName ] 
		thenCollect: [ :each | IceSavedPackageVersion savedPackage: self entry: each ]