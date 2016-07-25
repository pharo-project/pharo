menu-packages
demoteSelectedPackageAsPackageWithTag
	| package |
	package := self selectedPackage item.
	(package name includes: $-) ifFalse: [  ^ self inform: 'Package {1} does not have levels to downgrade.' ].
	package demoteToRPackageNamed: (package name copyUpToLast: $-)