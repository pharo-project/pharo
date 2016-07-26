groups
buildGroupManager
	| holder |
	holder := GroupHolder new.
	(holder addAPackageGroupSilentlyNamed: 'Work' packages: OrderedCollection new)
		removable: true;
		isFillable: true.
	(holder addAPackageGroupSilentlyNamed: self configurationsGroupName packages: self configurationPackages)
		removable: false;
		isFillable: true.
	^ holder