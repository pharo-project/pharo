accesing
selectedPackagesFrom: aBuilder
	| target selectedPackages |
	target := aBuilder model.
	selectedPackages := target selectedPackages collect: [ :rpackage | rpackage packageName asString ].
	^ selectedPackages