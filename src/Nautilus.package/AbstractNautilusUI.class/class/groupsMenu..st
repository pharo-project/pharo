menu
groupsMenu: aBuilder
	<contextMenu>
	<nautilusGlobalGroupMenu>
	| target |
	target := aBuilder model.
	target selectedGroup ifNil: [ ^ target ].
	(aBuilder item: #'Browse scoped')
		keyText: 'b, s' if: Nautilus useOldStyleKeys not;
		action: [ target restrictedBrowseGroups: target packageWidget selectedGroups ];
		order: 1000;
		withSeparatorAfter.
	(aBuilder item: #'Run tests')
		keyText: 'j, g' if: Nautilus useOldStyleKeys not;
		keyText: 't' if: Nautilus useOldStyleKeys;
		action: [ target runTestsOfGroups: target packageWidget selectedGroups notifying: true ];
		order: 1100.
	target selectedGroup
		ifNotNil: [ 
				(aBuilder item: #'Open group manager')
						action: [ target openGroupManager ];
						order: 1150.
			target selectedGroup isReadOnly
				ifFalse: [ 
					(aBuilder item: #'Rename...')
						keyText: 'r, g' if: Nautilus useOldStyleKeys not;
						keyText: 'r' if: Nautilus useOldStyleKeys;
						action: [ target renameGroup ];
						enabledBlock: [ target packageWidget selectedGroups size <= 1 ];
						order: 1200 ].
			target selectedGroup removable
				ifTrue: [ 
					(aBuilder item: #'Empty the group...')
						action: [ target selectedGroup removeAllPackages ];
						order: 1300.
					(aBuilder item: #'Remove...')
						keyText: 'x, g' if: Nautilus useOldStyleKeys not;
						keyText: 'x' if: Nautilus useOldStyleKeys;
						icon: (Smalltalk ui icons iconNamed: #removeIcon);
						action: [ target removeGroups ];
						order: 1400 ] ]