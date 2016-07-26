adding
addAPackageGroupSilentlyNamed: entry packages: aCollectionOfPackages
	(self includesAGroupNamed: entry)
		ifTrue: [ ^ self openError: entry name ]
		ifFalse: [ 
			| group |
			group := self groups add: (PackageGroup named: entry packages: aCollectionOfPackages).
			^ group ]