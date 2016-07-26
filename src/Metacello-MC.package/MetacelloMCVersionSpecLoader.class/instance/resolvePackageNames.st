loading
resolvePackageNames

	packages := Dictionary new.
	self resolveToLoadableSpecs.
	^packages values collect: [:pkg | pkg name ]