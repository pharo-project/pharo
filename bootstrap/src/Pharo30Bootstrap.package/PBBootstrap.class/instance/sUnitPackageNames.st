accessing
sUnitPackageNames

	sUnitPackageNames ifNotNil: [ ^ sUnitPackageNames ]. 
	self ensureBaselineOfPharoBootstrap.
	^ sUnitPackageNames := #BaselineOfPharoBootstrap asClass sUnitPackageNames
