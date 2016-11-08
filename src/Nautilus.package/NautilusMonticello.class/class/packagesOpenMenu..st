monticello menus
packagesOpenMenu: aBuilder
	<nautilusGlobalPackageMenu>
	| package mcpackage target last group |
	target := aBuilder model.
	(package := target selectedPackage) ifNil: [ ^ target ].
	mcpackage := package mcPackage.
	mcpackage ifNil: [ ^ self ].
	(aBuilder item: #('Open ' #, #package #name))
		label: 'Open...';
		order: 1297;
		help: 'Open';
		iconName: #monticelloPackageIcon;
		enabledBlock: [ target selectedPackages size < 2 ].
	(group := mcpackage workingCopy repositoryGroup) repositories
		doWithIndex: [ :repo :index | 
			last := (aBuilder item: 'Repo' , index printString)
				label: repo description;
				parent: #('Open ' #, #package #name);
				order: index;
				action: [ self default open: mcpackage in: repo ] ].
	last ifNotNil: [ :lastEntry | lastEntry withSeparatorAfter ].
	(aBuilder item: 'Add a repository')
		parent: #('Open ' #, #package #name);
		order: 99;
		action: [ self default addRepositoryTo: group ]