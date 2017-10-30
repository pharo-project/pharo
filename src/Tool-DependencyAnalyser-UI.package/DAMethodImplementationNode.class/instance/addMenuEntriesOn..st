building-menu
addMenuEntriesOn: aMenuGroup
	| method |
	method := self content.
	aMenuGroup
		addMenuItem:
			(MenuItemPresenter new
				name: 'Select this package as the right dependency';
				action:
					[(TheManifestBuilder of: self packageUnderAnalysis packageManifest)
						addManuallyResolvedDependency: method package name.
					"TODO: run the analysis again" ])