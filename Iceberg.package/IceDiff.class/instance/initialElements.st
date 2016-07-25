initialization
initialElements
	^ repository allPackages collect: [ :package |
			IcePackageChangeSet 
				package: package 
				parent: self ]