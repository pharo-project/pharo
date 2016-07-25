actions
revertSelection
	| loader |
	selection ifNotNil:
		[loader := MCPackageLoader new.
		selection inverse applyTo: loader.
		loader loadWithName: self changeSetNameForInstall ]