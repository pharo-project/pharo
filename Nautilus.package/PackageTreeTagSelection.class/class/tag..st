instance creation
tag: aTag 
	^ self node: ((PackageTreeTagNodeModel with: aTag)
		parentNode: (PackageTreePackageNodeModel with: aTag package);
		yourself)