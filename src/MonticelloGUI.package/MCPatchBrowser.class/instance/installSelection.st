actions
installSelection
	| loader |
	selection ifNotNil:
		[loader := MCPackageLoader new.
		selection applyTo: loader.
		loader loadWithName: self changeSetNameForInstall ]