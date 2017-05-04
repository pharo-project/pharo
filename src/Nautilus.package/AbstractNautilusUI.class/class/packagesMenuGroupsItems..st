menu
packagesMenuGroupsItems:aBuilder
	| target order |
	target := aBuilder model.
	order := 0.
	(aBuilder item: #'Add in Group...')
		order: 1600;
		help: 'Add current package in a group';
		enabledBlock:[target selectedPackage isPackageTag not].
	target groupsManager groups do: [ :group |  
		(aBuilder item: group name asSymbol)
			action: [ target addPackages: target selectedPackages inGroup: group ];
			parent: #'Add in Group...';
			order: order.
			order := order + 10
		].
	target parentOfSelectedPackage ifNil: [ ^ self ].
	(aBuilder item: #'Remove from Group')
		action: [ target removeFromGroupPackages: target selectedPackages ];
		order: 1650;
		help: 'Remove selected packages from group';
		withSeparatorAfter;
		enabledBlock:[target selectedPackage isPackageTag not & target parentOfSelectedPackage isNotNil].
