menu
packagesMenu: aBuilder
	<contextMenu>
	<nautilusGlobalPackageMenu>
	| package target |
	target := aBuilder model.
	(package := target selectedPackage) ifNil: [ ^ target ].
	(aBuilder item: #'Browse scoped')
		keyText: 'b, s' if: Nautilus useOldStyleKeys not;
		action: [ target restrictedBrowsePackage ];
		order: 1000;
		help: 'Open a browser on a restricted view';
		withSeparatorAfter.
	(aBuilder item: #'Rename...')
		keyText: 'r, p' if: Nautilus useOldStyleKeys not;
		keyText: 'r' if: Nautilus useOldStyleKeys;
		action: [ target renamePackage ];
		order: 1100;
		help: 'Rename a package';
		enabledBlock: [ target selectedPackages size < 2 ].
	(aBuilder item: #'Remove...')
		keyText: 'x, p' if: Nautilus useOldStyleKeys not;
		keyText: 'x' if: Nautilus useOldStyleKeys;
		action: [ target removePackages ];
		order: 1200;
		help: 'Remove a package';
		icon: (Smalltalk ui icons iconNamed: #removeIcon).
	(aBuilder item: #'Run tests...')
		keyText: 'j, p' if: Nautilus useOldStyleKeys not;
		keyText: 't' if: Nautilus useOldStyleKeys;
		action: [ target runPackagesTestsNotifying: true ];
		enabled: (package classes anySatisfy: [ :e | e isTestCase ]);
		order: 1250;
		help: 'Run tests';
		withSeparatorAfter.
	(aBuilder item: #'Promote as package')
		action: [ target promoteSelectedPackageTagAsPackage ];
		enabled: package isPackageTag;
		order: 1260;
		help: 'Promote this package tag as one package'.
	(aBuilder item: #'Demote to package with tag')
		action: [ target demoteSelectedPackageAsPackageWithTag ];
		enabled: (package isPackageTag not and: [ package name includes: $- ]);
		order: 1261;
		help: 'Downgrade this package as one package with a tag';
		withSeparatorAfter.
	(aBuilder item: #'File Out')
		action: [ target fileOutPackage ];
		order: 1300;
		help: 'File out a package';
		withSeparatorAfter.
	self packagesMenuGroupsItems: aBuilder